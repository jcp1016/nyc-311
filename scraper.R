require("rvest")
require("stringi")

smart_cities    <- html("http://datascience.columbia.edu/smart-cities")
affiliate_names <- html_nodes(smart_cities, "p:nth-child(5) a")
affiliates      <- html_nodes(smart_cities, "p:nth-child(5)")

#length(affiliates)

smart_cities %>%
        html_nodes("p:nth-child(5)") %>%
        html_tag(a)