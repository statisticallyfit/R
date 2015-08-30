setClass("Shape")
setClass("Polygon", representation(sides = "integer"), contains = "Shape")
setClass("Triangle", contains = "Polygon")
setClass("Square", contains = "Polygon")
setClass("Circle", contains = "Shape")

setGeneric("sides", valueClass="numeric", function(object) {
  standardGeneric("sides")
})

setMethod("sides", signature(object = "Polygon"), function(object){
  object@sides
})

setMethod("sides", signature("Triangle"), function(object) "3")
setMethod("sides", signature("Square"), function(object) 4)
setMethod("sides", signature("Circle"), function(object) Inf)

sides(new("Triangle")) # this raises error since triangle output is string

# fix it: 
setMethod("sides", signature("Triangle"), function(object) 3)
sides(new("Triangle"))




# to show what methods are already defined for a generic function: 
showMethods("sides")
showMethods(class="Polygon")
