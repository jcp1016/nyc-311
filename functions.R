tractLookup <- function(x, y, state) {
        pt <- SpatialPoints(data.frame(x = x, y = y))
        overlay.pt <- overlay(pt, state) # what index number does pt fall inside?
        return(census$TRACT[overlay.pt]) # give the Tract number from the census layer
}