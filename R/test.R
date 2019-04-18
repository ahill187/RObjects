generator_funs <- list()
list2env2 <- function(x, envir = NULL, parent = emptyenv(),
                      hash = (length(x) >  100),
                      size = max(29L, length(x)),
                      empty_to_null = TRUE) {
    if (is.null(envir)) {
        envir <- new.env(hash = hash, parent = parent, size = size)
    }
    if (length(x) == 0) {
        if (empty_to_null)
            return(NULL)
        else
            return(envir)
    }
    list2env(x, envir)
}
generator_funs$new <- function(...) {
    # Get superclass object -------------------------------------------
    inherit <- get_inherit()

    # Some checks on superclass ---------------------------------------
    if (!is.null(inherit)) {
        if (!inherits(inherit, "R6ClassGenerator"))
            stop("`inherit` must be a R6ClassGenerator.")

        if (!identical(portable, inherit$portable))
            stop("Sub and superclass must both be portable or non-portable.")

        # Merge fields over superclass fields, recursively --------------
        recursive_merge <- function(obj, which) {
            if (is.null(obj)) return(NULL)
            merge_vectors(recursive_merge(obj$get_inherit(), which), obj[[which]])
        }
        public_fields  <- merge_vectors(recursive_merge(inherit, "public_fields"),
                                        public_fields)
        private_fields <- merge_vectors(recursive_merge(inherit, "private_fields"),
                                        private_fields)
    }

    if (class) {
        classes <- c(classname, get_superclassnames(inherit), "R6")
    } else {
        classes <- NULL
    }

    # Precompute some things ------------------------------------------
    has_priv <- has_private()


    # Create binding and enclosing environments -----------------------
    if (portable) {
        # When portable==TRUE, the public binding environment is separate from the
        # enclosing environment.

        # Binding environment for private objects (where private objects are found)
        if (has_priv)
            private_bind_env <- new.env(parent = emptyenv(), hash = FALSE)
        else
            private_bind_env <- NULL

        # Binding environment for public objects (where public objects are found)
        public_bind_env <- new.env(parent = emptyenv(), hash = FALSE)

        # The enclosing environment for methods
        enclos_env <- new.env(parent = parent_env, hash = FALSE)

    } else {
        # When portable==FALSE, the public binding environment is the same as the
        # enclosing environment.
        # If present, the private binding env is the parent of the public binding
        # env.
        if (has_priv) {
            private_bind_env <- new.env(parent = parent_env, hash = FALSE)
            public_bind_env <- new.env(parent = private_bind_env, hash = FALSE)
        } else {
            private_bind_env <- NULL
            public_bind_env <- new.env(parent = parent_env, hash = FALSE)
        }

        enclos_env <- public_bind_env
    }

    # Add self and private pointer ------------------------------------
    enclos_env$self <- public_bind_env
    if (has_priv)
        enclos_env$private <- private_bind_env

    # Fix environment for methods -------------------------------------
    public_methods <- assign_func_envs(public_methods, enclos_env)
    if (has_priv)
        private_methods <- assign_func_envs(private_methods, enclos_env)
    if (!is.null(active))
        active <- assign_func_envs(active, enclos_env)

    # Enable debugging ------------------------------------------------
    if (length(debug_names) > 0) {
        lapply(public_methods[names(public_methods) %in% debug_names], base::debug)
        lapply(private_methods[names(private_methods) %in% debug_names], base::debug)
        lapply(active[names(active) %in% debug_names], base::debug)
    }

    # Set up superclass objects ---------------------------------------
    if (!is.null(inherit)) {
        if (portable) {
            # Set up the superclass objects
            super_struct <- create_super_env(inherit, public_bind_env,
                                             private_bind_env, portable = TRUE)
        } else {
            # Set up the superclass objects
            super_struct <- create_super_env(inherit, public_bind_env, portable = FALSE)
        }

        enclos_env$super <- super_struct$bind_env

        # Merge this level's methods over the superclass methods
        public_methods  <- merge_vectors(super_struct$public_methods, public_methods)
        private_methods <- merge_vectors(super_struct$private_methods, private_methods)
        active          <- merge_vectors(super_struct$active, active)
    }

    # Copy objects to public bind environment -------------------------
    list2env2(public_methods, envir = public_bind_env)
    list2env2(public_fields, envir = public_bind_env)

    # Copy objects to private bind environment ------------------------
    if (has_priv) {
        list2env2(private_methods, envir = private_bind_env)
        list2env2(private_fields, envir = private_bind_env)
    }

    # Set up active bindings ------------------------------------------
    if (!is.null(active)) {
        for (name in names(active)) {
            makeActiveBinding(name, active[[name]], public_bind_env)
        }
    }

    # Add refs to other environments in the object --------------------
    public_bind_env$`.__enclos_env__` <- enclos_env

    # Lock ------------------------------------------------------------
    if (lock_objects) {
        if (has_priv) lockEnvironment(private_bind_env)
        lockEnvironment(public_bind_env)
    }

    # Always lock methods
    if (has_priv) {
        for (name in names(private_methods))
            lockBinding(name, private_bind_env)
    }
    for (name in names(public_methods))
        lockBinding(name, public_bind_env)

    class(public_bind_env) <- classes

    # Initialize ------------------------------------------------------
    if (is.function(.subset2(public_bind_env, "initialize"))) {
        .subset2(public_bind_env, "initialize")(...)
    } else if (length(list(...)) != 0 ) {
        stop("Called new() with arguments, but there is no initialize method.")
    }

    # Finalizer -------------------------------------------------------
    if (is.function(.subset2(public_bind_env, "finalize"))) {
        # This wraps the user's `finalize` method. The user's finalize method
        # typically does not have an `e` argument, so the wrapper needs to consume
        # the `e` argument.
        finalizer_wrapper <- function(e) {
            .subset2(e, "finalize")()
        }
        # Reassign the wrapper's environment so that it does not capture the current
        # environment and prevent objects from getting GC'd.
        environment(finalizer_wrapper) <- baseenv()

        reg.finalizer(
            public_bind_env,
            finalizer_wrapper,
            onexit = TRUE
        )
    }

    if (has_priv) {
        if (is.function(.subset2(private_bind_env, "finalize"))) {
            finalizer_wrapper <- function(e) {
                .subset2(e, ".__enclos_env__")$private$finalize()
            }
            environment(finalizer_wrapper) <- baseenv()
            reg.finalizer(
                public_bind_env,
                finalizer_wrapper,
                onexit = TRUE
            )
        }
    }

    public_bind_env
}



# This function returns the superclass object
generator_funs$get_inherit <- function() {
    # The NULL arg speeds up eval a tiny bit
    eval(inherit, parent_env, NULL)
}

# This is the $has_private function for a R6ClassGenerator. This copy of it
# won't run properly; it needs to be copied, and its parent environment set to
# the generator object environment.
# Returns TRUE if this class or one of its ancestor superclasses has private
# members; FALSE otherwise.
generator_funs$has_private <- function() {
    inherit <- get_inherit()
    if (!is.null(private_fields) || !is.null(private_methods))
        TRUE
    else if (is.null(inherit))
        FALSE
    else
        inherit$has_private()
}

# This is the $set function for a R6ClassGenerator. This copy of it won't run
# properly; it needs to be copied, and its parent environment set to the
# generator object environment.
generator_funs$set <- function(which = NULL, name = NULL, value, overwrite = FALSE) {
    if (lock_class)
        stop("Can't modify a locked R6 class.")

    if (is.null(which) || !(which %in% c("public", "private", "active")))
        stop("`which` must be 'public', 'private', or 'active'.")

    if (is.null(name) || !is.character(name))
        stop("`name` must be a string.")

    if (missing(value))
        stop("`value` must be provided.")

    # Find which group this object should go in.
    if (which == "public") {
        group <- if (is.function(value)) "public_methods" else "public_fields"
    } else if (which == "private") {
        group <- if (is.function(value)) "private_methods" else "private_fields"
    } else if (which == "active") {
        if (is.function(value))
            group <- "active"
        else
            stop("Can't add non-function to active")
    }

    # Check that it's not already present
    all_groups <- c("public_methods", "public_fields", "private_methods",
                    "private_fields", "active")

    # If we're allowed to overwrite, don't check the group that this object
    # would go in.
    if (overwrite)
        all_groups <- setdiff(all_groups, group)

    all_names <- unlist(lapply(all_groups, function(g) names(get(g))))

    if (name %in% all_names) {
        stop("Can't add ", name, " because it already present in ", classname,
             " generator.")
    }

    # Assign in correct group. Create group if it doesn't exist.
    if (is.null(self[[group]]))
        self[[group]] <- list()

    if (is.null(value)) {
        # If it's NULL, the item should get a NULL value. The `[[<-` assignment
        # would instead delete the item; this method gives it a NULL value.
        self[[group]][name] <- list(NULL)
    } else {
        self[[group]][[name]] <- value
    }

    invisible()
}


# Enable debugging for one or more methods. This will apply to all objects
# instantiated after this is called.
generator_funs$debug <- function(name) {
    debug_names <<- union(debug_names, name)
}




capsule <- new.env(hash = FALSE)
attr(capsule, "name") <- "test_capsule"

# This function takes an expression and evaluates it in the capsule environment.
encapsulate <- function(expr) {
    expr <- substitute(expr)
    eval(expr, capsule)
}

all_named <- function(x) {
    if (length(names(x)) != length(x) || any(names(x) == "")) {
        return(FALSE)
    }
    TRUE
}
# Return all the functions in a list.
get_functions <- function(x) {
    funcs <- vapply(x, is.function, logical(1))
    if (all(!funcs)) return(NULL)
    x[funcs]
}

# Return all the non-functions in a list.
get_nonfunctions <- function(x) {
    funcs <- vapply(x, is.function, logical(1))
    if (all(funcs)) return(NULL)
    x[!funcs]
}

assign_func_envs <- function(objs, target_env) {
    if (is.null(target_env)) return(objs)

    lapply(objs, function(x) {
        if (is.function(x)) environment(x) <- target_env
        x
    })
}
a=function(t){
    a.test = function(t){
        print(t)
    }
}

notBoth = function(A, B) {
    notA = setdiff(B, A)
    notB = setdiff(A, B)
    neither = c(notA, notB)
    return(neither)
}
object = encapsulate(function(type = NULL,
                              access = "public",
                              parent_env = parent.frame()){
    generator = list()
    generator$type = type
    generator$previousEnv = ls(parent.frame())
    accessOptions = c("public", "private", "protected")
    access = match.arg(access, accessOptions, several.ok = FALSE)
    generator$new = function(value) {
        instantiatedObj = notBoth(generator$previousEnv, ls(parent.frame()))
        if(length(instantiatedObj)==0){
            stop("Object with same name already created in this environment")
        }
        print(instantiatedObj)
        if(class(value)!=type) {
            stop("Instantiated type must match object type")
        } else {
            env_poke(parent.frame(), instantiatedObj, value)
        }
    }
    generator
})

test2 = function(className, ...){

    args = list(...)
    print(args)
    for(arg in args) {
        if(is.function(arg)){
            print("function")
        }
    }
}

test <- encapsulate(function(classname = NULL, public = list(),
                                private = NULL, active = NULL,
                                inherit = NULL, lock_objects = TRUE,
                                class = TRUE, portable = TRUE,
                                lock_class = FALSE, cloneable = TRUE,
                                parent_env = parent.frame(), lock) {

    if (!all_named(public) || !all_named(private) || !all_named(active))
        stop("All elements of public, private, and active must be named.")

    allnames <- c(names(public), names(private), names(active))
    if (any(duplicated(allnames)))
        stop("All items in public, private, and active must have unique names.")

    if ("clone" %in% allnames)
        stop("Cannot add a member with reserved name 'clone'.")

    if (any(c("self", "private", "super") %in%
            c(names(public), names(private), names(active))))
        stop("Items cannot use reserved names 'self', 'private', and 'super'.")

    if ("initialize" %in% c(names(private), names(active)))
        stop("'initialize' is not allowed in private or active.")

    if (length(get_nonfunctions(active)) != 0)
        stop("All items in active must be functions.")

    if (!missing(lock)) {
        message(paste0(
            "R6Class ", classname, ": 'lock' argument has been renamed to 'lock_objects' as of version 2.1.",
            "This code will continue to work, but the 'lock' option will be removed in a later version of R6"
        ))
        lock_objects <- lock
    }

    # Create the generator object, which is an environment
    generator <- new.env(parent = capsule)

    generator$self <- generator

    # Set the generator functions to eval in the generator environment, and copy
    # them into the generator env.
    generator_funs <- assign_func_envs(generator_funs, generator)
    list2env2(generator_funs, generator)

    generator$classname    <- classname
    generator$active       <- active
    generator$portable     <- portable
    generator$parent_env   <- parent_env
    generator$lock_objects <- lock_objects
    generator$class        <- class
    generator$lock_class   <- lock_class

    # Separate fields from methods
    generator$public_fields   <- get_nonfunctions(public)
    generator$private_fields  <- get_nonfunctions(private)
    generator$public_methods  <- get_functions(public)
    generator$private_methods <- get_functions(private)

    if (cloneable)
        generator$public_methods$clone <- generator_funs$clone_method

    # Capture the unevaluated expression for the superclass; when evaluated in
    # the parent_env, it should return the superclass object.
    generator$inherit <- substitute(inherit)

    # Names of methods for which to enable debugging
    generator$debug_names <- character(0)

    attr(generator, "name") <- paste0(classname, "_generator")
    class(generator) <- "R6ClassGenerator"

    generator
})


ThisIsATest <- test(
    "ThisIsATest",
    public = list(

        # Fields -----

        # Generic
        mainBoxColor = "info",
        # Graph
        graphInputId = NULL,
        graphOutputId = NULL,
        googleChartOutputId = NULL,
        # Input for Graph
        dataInput = c("userInput", "csvInput", "functionInput"),
        # Dropdown for Graph
        dropdownId = NULL,
        dropdownChoices = NULL,
        dropdownSelected = NULL,
        dropdown = TRUE,
        # Sidebar
        sidebarChoicesNumber = 4,
        sidebarShownLabels = NULL,
        sidebarShownChoices = c(),
        sidebarHiddenChoices = c(),
        sidebarShownSelected = NULL,
        sidebarHiddenSelected = NULL,
        sidebarShownIds = NULL,
        sidebarHiddenIds = NULL,
        sidebarHiddenBoxIds = NULL,
        # Download
        downloadLabel = "Download",
        downloadInputId = NULL,
        downloadOutputId = NULL,
        pngDownloadName = NULL,
        # Server Output
        outputTypes = c("googleChartOutput", "downloadOutput"),
        outputIds = c(),
        chartType = "googleChart",
        # App Bricks
        brickTypes = c("sidebarInput", "googleChartOutput", "downloadOutput"),


        # Constructor
        initialize = function(
            title,
            inputId,
            mainBoxColor,
            tabNumber,
            dropdownChoices,
            dropdownSelected,
            dropdown = TRUE,
            sidebarChoicesNumber,
            sidebarShownLabels,
            downloadLabel = "Download",
            pngDownloadName,
            dataSubClasses,
            columnOptions,
            columnTypes

        ){}
    )
)

parentls <- function() {
    ls(envir=parent.frame())
}

a<-function() {
    x <- 5
    parentls()
}

b <- function() {
    z <- 10
    parentls()
}


