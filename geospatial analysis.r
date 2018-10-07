
library(tidyverse)

library(sf) # to analyse geospatial data + vector data
library(raster) # analyse geospatial data + raster data
library(spData) # load geographic data
library(spDataLarge) # load geographic data (larger)
library(sp) # for dealing with spatial data (older version of sf, but other packages still only support this format)

#
# 2. Geographic Data in R ######################################
## sf = data type that stores information as both data frames and geometry type of data (geom).
# this package incorporate packages such as sp (for class system), rfdal (for reading and writing data), and rgeos (for spatioal operation)

# we will use the 'world' dataset from spData
names(world)

plot(world)

summary(world["lifeExp"]) # the geom object is sticky!

# sf objects are easy to subset
world_mini = world[1:2, 1:3]
world_mini

# even though sf is very versatile, many packages still can only deal with spatial data
# we can convert them using 'sp' package
require(sp)

world_sp = as(world, Class = "Spatial")

# and can be converted back to 'simple feature' object
world_sf = st_as_sf(world_sp, "sf")


# 2.1 Basic map making ============================
plot(world["pop"])

# combines countries in Asia
world_asia = world[world$continent == "Asia", ]
asia = st_union(world_asia)

# we can now plot ASian continent over a map of the world
plot(world["pop"], reset = FALSE) # to prevent reseting the existing plot
plot(asia, add = TRUE, col = "red") # to add something to existing plot

# but later we will use more advance map making tool (tmap package)

plot(world["continent"], reset = FALSE)
cex = sqrt(world$pop)/10000
world_cents = st_centroid(world, of_largest = TRUE)
plot(st_geometry(world_cents), add= TRUE, cex = cex) # add circles depending on population size


# st_centroid is used to convert one geometry type (polygons) to another (points)

# bold a certain object using expandBB (bottom ,left, top, right)
india = world[world$name_long == "India",]
plot(st_geometry(india), expandBB = c(0, 0.2, 0.1, 1), col = "blue", lwd = 3)
plot(world_asia[0], add = TRUE) # [0] to keep only the geometry column

#
# 2.2 Vector Data ======================================
# using a coordinate reference system (CRS)

# 2.2.1 Geometry types ==================================

# 7 most commonly used types:
# POINT, LINESTRING, POLYGON, MULTIPOINT, MULTILINESTRING, MULTIPOLYGON, and GEOMETRYCOLLECTION
# generally known as well-known binary (WKB) or well-known text (WKT)

## POINT(5 2)
## LINESTRING (1 5, 4 4, 4 1, 2 2, 3 2)
## POLYGON ((1 5, 2 2, 4 1, 1 5)) <<< first and last coordinate need to be the same
# one simple feature contains 1 geometric object
# one feature can contain multiple geometric objects with 'multi' version (e.g. MULTIPOINT(x y, x2 y2, etc.))
# one feature can contain collections of geometric objects 'geometrycollection' version
#     (e.g. GEOMETRYCOLLECTION (MULTIPOINT (x1 y1, x2 y2), LINESTRING(x1 y1, x2 y2, x3 y3)))


# 2.2.2 Simple feature geometries (sfg) ==============================

# sfg class = represents simple feature geometry types in R
# the commands are: st_point(), st_polygon(), st_multilinestring(), st_geometrycollection()

# sfg objects cna be created from three native data types:
# 1. a numeric vector
# 2. a matrix
# 3. a list

st_point(c(5,2))
st_point(c(5,2,3))
st_point(c(5,2,3), dim = "XYM")
st_point(c(5,2,3,1))

# multipoint
st_multipoint( rbind(c(5,2), c(1,3), c(3,4), c(3,2)))

# polygon with a hole!
st_polygon(
            list(
                rbind(c(1,5), c(2,2), c(4,1), c(4,4), c(1,5)), # outer border
                rbind(c(2,4), c(3,4), c(3,3), c(2,3), c(2,4))  # inner hole
                )
           )

# you can wrap this around again with other shapes with st_geometrycollection
# just make sure to use list()

# 2.2.3 Simple feature columns (sfc) ================================

point1 = st_point(c(5,2))
point2 = st_point(c(1,3))
points_sfc = st_sfc(point1, point2)
points_sfc

# in most cases, an sfc object contains objects of the same geometry type
polygon_list1 = list(rbind(c(1, 5), c(2, 2), c(4, 1), c(4, 4), c(1, 5)))
polygon1 = st_polygon(polygon_list1)
polygon_list2 = list(rbind(c(0, 2), c(1, 2), c(1, 3), c(0, 3), c(0, 2)))
polygon2 = st_polygon(polygon_list2)
polygon_sfc = st_sfc(polygon1, polygon2)

# to check the types of objects in the sfc
st_geometry_type(polygon_sfc) # this sfc object contains 2 polygons

# sfc MULTILINESTRING
multilinestring_list1 = list(rbind(c(1, 5), c(4, 4), c(4, 1), c(2, 2), c(3, 2)), 
                             rbind(c(1, 2), c(2, 4)))
multilinestring1 = st_multilinestring((multilinestring_list1))
multilinestring_list2 = list(rbind(c(2, 9), c(7, 9), c(5, 6), c(4, 7), c(2, 7)), 
                             rbind(c(1, 7), c(3, 8)))
multilinestring2 = st_multilinestring((multilinestring_list2))
multilinestring_sfc = st_sfc(multilinestring1, multilinestring2)

# it is possible to create an sfc object from sfg with different geometry types:
point_multilinestring_sfc = st_sfc(point1, multilinestring1)
st_geometry_type(point_multilinestring_sfc) # a point and a multistring :D


## sfc can store coordinate reference system (default vaule = NA)
st_crs(points_sfc)

# we can add CRS system to sfc
points_sfc_wgs = st_sfc(point1, point2, crs = 4326)
st_crs(points_sfc_wgs)

# it also accepts a raw proj4string:
st_sfc(point1, point2, crs = "+proj=longlat +datum=WGS84 +no_defs")
# same EPGS ID


# 2.2.4 The sf class (sf) ======================================

# this combines sfc data type with data.frame that contains non-geometric information
lnd_point = st_point(c(0.1, 51.5))                # sfg object
lnd_geom = st_sfc(lnd_point, crs = 4326)          # sfc object
lnd_attrib = data.frame(
  name = "London",
  temperature = 25,
  date = as.Date("2017-06-21")
)

lnd_sf = st_sf(lnd_attrib, geometry = lnd_geom)   # sf object
lnd_sf
class(lnd_sf)

#
# 2.3 Raster data ============================

# The geographic raster data model usually consists of a raster header and a matrix (with rows and columns)

# this type of data makes processing much more efficient and faster than vector data
# but, they can only hold a single value for one cell of raster layer.

require(spDataLarge)
require(rgdal)

# you can convert .tif into raster object using raster()
# this example here is already a raster object
nz_elev

plot(nz_elev)

brick(nz_elev) # this one only contains one layer
# brick() can transform .tif into multi-layers raster object (known as RasterBrick)

# 2.4 Coordinate Reference System (CRS) ======================

# The Projected CRS are based on Catersian coordinates on an implicitly flat surface.
# The coordinates contain two values: Easting and Northing (x and y)

crs_data = rgdal::make_EPSG()
View(crs_data)


st_crs(world) # checking existing crs system
st_set_crs(world_mini, 26912) # lol, cannot set crs for an sf file with existing crs

projection(nz_elev) # check crs system from raster object

# you can set crs using the same command
projection(nz_elev) = "+proj=tmerc +lat_0=0 +lon_0=173 +k=1 +x_0=1 +y_0=0.2
                      +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"

## UNITS
luxembourg = world[world$name_long == "Luxembourg", ]
st_area(luxembourg)


#
# 3. Attribute Data Operation #########################

dim(world) # two dimensional object with rows and columns
nrow(world)
ncol(world)

world_df = st_set_geometry(world, NULL) # remove geometry attribute
class(world_df)


# subsetting
small_countries = world[world$area_km2 < 10000, ]

# select
world1 = dplyr::select(world, name_long, pop)

# data frame object
world[, "pop"]

# vector objects
pull(world, pop)

# select row = slice()
slice(world, 3:5)


## using dplyr pipe operator
world7 = world %>%
  filter(continent == "Asia") %>%
  dplyr::select(name_long, continent) %>%
  slice(1:5)

world7


## aggregation!
world_agg3 = world %>%
  group_by(continent) %>%
  summarise(pop = sum(pop, na.rm = TRUE))

world_agg3

### Manipulating raster object

# let's create a raster object
elev = raster(nrows = 6, ncols = 6, res = 0.5,
              xmn = -1.5, xmx = 1.5, ymn = -1.5, ymx = 1.5, vals = 1:36)
# so, we just created a raster object with 6 rows and 6 columns and the spatial extent is defined by x y direction
# vals is an argument to popualte the raster with values (1 to 36 integer in this case)
plot(elev)

# we can also populate the values with categorical data
grain_order = c("clay", "slit", "sand")
grain_char = sample(grain_order, 36, replace = TRUE)
grain_fact = factor(grain_char, levels = grain_order)
grain = raster(nrows = 6, ncols = 6, res = 0.5,
               xmn = -1.5, xmx = 1.5, ymn = -1.5, ymx = 1.5, vals = grain_fact)
plot(grain)


# raster subsetting
elev[1,1]
elev[3]


# create a RasterStack object
r_stack = stack(elev, grain)
names(r_stack) = c("elev", "grain")

r_stack$elev

# summarising raster objects
cellStats(elev, sd)

hist(elev)
#

# 4. Spatial Data Operations ####################

# spatial subsetting
canterbury = nz %>% filter(Name == "Canterbury")
canterbury_height = nz_height[canterbury, ]

plot(canterbury)

plot(nz_height[canterbury, 2 , op = st_disjoint]) # op = st_disjoint >> includes objects that do not intersect with Canterbury


# topological operators
sel_sgbp = st_intersects(x = nz_height, y = canterbury)
class(sel_sgbp)
sel_logical = lengths(sel_sgbp) > 0
canterbury_height2 = nz_height[sel_logical, ]
plot(canterbury_height2)

canterbury_height3 = nz_height %>%
  filter(st_intersects(x = ., y = canterbury, sparse = FALSE))


# 4.1 Topological relations ===================

# example with simple geometry objects
a_poly = st_polygon(list(rbind(c(-1, -1), c(1,-1), c(1,1), c(-1,-1))))
a = st_sfc(a_poly) # a polygon

l_line = st_linestring(x = matrix(c(-1, -1, -0.5, 1), ncol = 2))
l = st_sfc(l_line) # a line

p_matrix = matrix(c(0.5, 1, -1, 0, 0, 1, 0.5, 1), ncol =2)
p_multi = st_multipoint(x = p_matrix)
p = st_cast(st_sfc(p_multi), "POINT") # multi points

st_intersects(p, a, sparse = FALSE) # points that intersect with the polygon
st_disjoint(p, a, sparse = FALSE) # points that are outside the polygon
st_within(p, a, sparse = FALSE) # points that are inside the polygon (those in the border are excluded)
st_touches(p, a, sparse = FALSE) # points that touch the border of the polygon

# plot the shapes
plot(a)
plot(l, add = TRUE)
plot(p, add = TRUE)

# you can also do st_is_within_distance


# 4.2 Spatial joining ===========================

set.seed(2018)
bb_world = st_bbox(world) # set a boundary box

random_df = tibble(
  x = runif(n = 10, min = bb_world[1], max = bb_world[3]),
  y = runif(n = 10, min = bb_world[2], max = bb_world[4])
)

random_points = random_df %>%
  st_as_sf(coords = c("x", "y")) %>% # set coordinates
  st_set_crs(4326)

world$name_long = as.character(world$name_long)
world_random = world[random_points, ]
random_joined = st_join(random_points, world["name_long"])
random_joined


# 4.3 Non-overlapping joins =============================
data(cycle_hire)
data(cycle_hire_osm)

plot(cycle_hire$geometry, col = "blue")
plot(cycle_hire_osm$geometry, col = "red", pch = 3, add = TRUE)

any(st_touches(cycle_hire, cycle_hire_osm, sparse = FALSE)) # any of the coordinate touches?


# transform the data into projected crs
cycle_hire_p = st_transform(cycle_hire, 27700)
cycle_hire_osm_p = st_transform(cycle_hire_osm, 27700)

# create a logic that tells if the point in left sf are within 20m of the right sf
sel = st_is_within_distance(cycle_hire_p, cycle_hire_osm_p, dist = 20) # within 20m of adjacent points
summary(lengths(sel) > 0)

# we can do st_join to retrieve values associated with cycle_hire_osm_p
z = st_join(cycle_hire_p, cycle_hire_osm_p, st_is_within_distance, dist = 20)
nrow(cycle_hire)
nrow(z) # hmmm.. overlaps... some hire stations in cycle_hire_P have multiple matches in cycle_hire_osm_P

# we need aggregation
z = z %>%
  group_by(id) %>%
  summarise(capacity = mean(capacity))
nrow(z) == nrow(cycle_hire)

plot(cycle_hire_osm["capacity"])
plot(z["capacity"])


# 4.4 Spatial data aggregation ===========================

wnzh = st_sf(nz_height)
wnz = st_sf(nz)

nz_avheight = aggregate(x = wnzh, by = wnz, FUN = mean)

