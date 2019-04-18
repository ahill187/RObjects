notBoth = function(A, B) {
    notA = setdiff(B, A)
    notB = setdiff(A, B)
    neither = c(notA, notB)
    return(neither)
}
capsule <- new.env(hash = FALSE)
attr(capsule, "name") <- "test_capsule"

# This function takes an expression and evaluates it in the capsule environment.
encapsulate <- function(expr) {
    expr <- substitute(expr)
    eval(expr, capsule)
}
#' @title object
#' @param type class type, e.g. character, numeric
#' @param access class access, public, private, or protected (as in Java)
#' @import lobstr rlang
#' @export
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