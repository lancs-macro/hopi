# confirm that it’s able to connect to your DigitalOcean account.
analogsea::droplets()

# will start a virtual machine (or “droplet”, as DigitalOcean calls them) and 
# install Plumber and all the necessary prerequisite software.
mydrop <- plumber::do_provision(droplet = "hprice-index")

analogsea::droplet_delete(as.droplet("CleansedArchipelago"))

# Install any R packages on the server that your API requires using 
analogsea::install_r_package(droplet = , package = "plumber")
