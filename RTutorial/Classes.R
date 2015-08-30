
# Create the Agent class (base)

Agent <- setClass(
  # Set the name
  "Agent", 
  
  # Define the slots
  slots = c(
    location = "numeric", 
    velocity = "numeric", 
    active = "logical"
  ), 
  
  # Set the default values for the slots (optional)
  prototype = list(
    location = c(0.0, 0.0), 
    active = TRUE, 
    velocity = c(0.0, 0.0)
  ),
  
  # Make a function to test if data is consistent - this isn't
  # called if there is an initialize function defined
  validity = function(object) {
    if(sum(object@velocity^2) > 100.0) {
      return("The velocity level is out of bounds.")
    }
    return(TRUE)
  }
)






# ------------- Test it ----------------
a <- Agent()
a
is.object(a)
isS4(a)

slotNames(a)
slotNames("Agent") # same things

# getSlots(a) # produces error
getSlots("Agent") # not the same

getClass(a)
getClass("Agent")  # also not the same

slot(a, "location")
slot(a, "location") <- c(1,5) 
a






# ----------- Add functions to the class ---------------

# create a method to assign the value of the location
setGeneric(name="setLocation",
           def=function(theObject,position)
           {
             standardGeneric("setLocation")
           }
)

setMethod(f="setLocation",
          signature="Agent",
          definition=function(theObject,position)
          {
            theObject@location <- position
            validObject(theObject)
            return(theObject)
          }
)

# create a method to get the value of the location
setGeneric(name="getLocation",
           def=function(theObject)
           {
             standardGeneric("getLocation")
           }
)

setMethod(f="getLocation",
          signature="Agent",
          definition=function(theObject)
          {
            return(theObject@location)
          }
)


# create a method to assign the value of active
setGeneric(name="setActive",
           def=function(theObject,active)
           {
             standardGeneric("setActive")
           }
)

setMethod(f="setActive",
          signature="Agent",
          definition=function(theObject,active)
          {
            theObject@active <- active
            validObject(theObject)
            return(theObject)
          }
)

# create a method to get the value of active
setGeneric(name="getActive",
           def=function(theObject)
           {
             standardGeneric("getActive")
           }
)

setMethod(f="getActive",
          signature="Agent",
          definition=function(theObject)
          {
            return(theObject@active)
          }
)


# create a method to assign the value of velocity
setGeneric(name="setVelocity",
           def=function(theObject,velocity)
           {
             standardGeneric("setVelocity")
           }
)

setMethod(f="setVelocity",
          signature="Agent",
          definition=function(theObject,velocity)
          {
            theObject@velocity <- velocity
            validObject(theObject)
            return(theObject)
          }
)

# create a method to get the value of the velocity
setGeneric(name="getVelocity",
           def=function(theObject)
           {
             standardGeneric("getVelocity")
           }
)

setMethod(f="getVelocity",
          signature="Agent",
          definition=function(theObject)
          {
            return(theObject@velocity)
          }
)



# ------------- Test it --------------
a <- Agent(c(1, 5), FALSE, c(-2, 3))
getVelocity(a)
a <- setVelocity(a, c(1.0, 2.0))
a <- setLocation(a, c(5, 4))
getLocation(a)
