source(file.path("R", "data.R"))

customers <- generate_customers()
path <- save_customers(customers)
message("Saved ", nrow(customers), " customers to ", normalizePath(path))
