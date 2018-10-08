
library(tidyverse)

library(sf) # to analyse geospatial data + vector data
library(raster) # analyse geospatial data + raster data
library(spData) # load geographic data
library(spDataLarge) # load geographic data (larger)
library(sp) # for dealing with spatial data (older version of sf, but other packages still only support this format)
library(rmapshaper) # ms_simplify (using different algorithm to maintain shape)

library(rnaturalearth) # to get various vector or raster geospatial data

library(tmap) # to plot map

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


# 4.1 Spatial operations on vector data ################
# 4.1.1 Topological relations ===================

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


# 4.1.2 Spatial joining ===========================

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


# 4.1.3 Non-overlapping joins =============================
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


# 4.1.4 Spatial data aggregation ===========================

wnzh = st_sf(nz_height)
wnz = st_sf(nz)

nz_avheight = aggregate(x = wnzh, by = wnz, FUN = mean) # use the same geometry as the (spatial) aggregating object (wnz)

plot(nz_avheight[, "elevation"])

# or using tidyverse
nz_avheight2 = nz %>%
  st_join(nz_height) %>%
  group_by(Name) %>%
  summarise(elevation = mean(elevation, na.rm = TRUE))

plot(nz_avheight2[, "elevation"])

# 4.1.5 Incongruent data aggregation --------------------------

# spatial aggregation can only work if the geometries are congruent with each other

plot(incongruent)
plot(aggregating_zones) # this aggregating zones are not congruent with the incongruent dataset

# we can do area-weighted spatial interpolation
# values from the incongruent object are allocated to the aggregating zones proportional to the area

agg_aw = st_interpolate_aw(incongruent[, "value"], aggregating_zones, extensive = TRUE)
agg_aw$value

plot(agg_aw[, "value"])

# In this case, it is meaningful to sum up the values of the intersections falling into the aggregating zones
# since total income is a so-called spatially extensive variable.
# This would be different for spatially intensive variable, such as income per head / percentages.
# It is more meaningful to apply an average function when doing the aggregation instead of sum function.
# to do so, set the extensive parameter to FALSE.


# 4.1.6 Distance Relations ===============================

nz_highest = nz_height %>% top_n(n = 1, wt = elevation)
canterbury_centroid = st_centroid(canterbury)

st_distance(nz_highest, canterbury_centroid) # distance between the centroid of canterbury and the nearest border with the highest elevation

# st_distance can also calculate distance of multiple objects
co = filter(nz, grepl("Canter|Otag", Name))
st_distance(nz_height[1:5, ], co) # first 5 points and their distance to the nearest border of the geometry object

plot(st_geometry(co)[2])
plot(st_geometry(nz_height)[2:3], add = TRUE) # will result in 0 if the points are inside the border

wnz = wnz %>%
  mutate(centroid = st_centroid(geom)) # ooo, I can calculate the centroid of each polygon :D

#
# 4.2 Spatial operations on raster data ===========================
# 4.2.1 Spatial subsetting ----------------------------

# get a value from a cell located X away and Y away from the origin
id = cellFromXY(elev, xy = c(0.1, 0.1))
elev[id]

raster::extract(elev, data.frame(x = 0.1, y = 0.1)) # same as above

# raster object can also be subsetted with another raster object
clip = raster(nrows = 3, ncols = 3, res = 0.3, xmn = 0.9, xmx = 1.8, ymn = -0.45, ymx = 0.45, vals = rep(1,9))
plot(elev)
plot(clip, add = TRUE)
elev[clip] # return values that intersect with the second raster object


# we can also get spatial object as output
plot(elev[1:9, drop = FALSE]) # row ID, from top left to right then top left second row to right and so on
plot(elev[1, 1:3, drop = FALSE]) # row, column


# using raster mask
rmask = elev
values(rmask) = sample(c(NA, TRUE), 36, replace = TRUE)

# all these three code lines get the same result
plot(elev[rmask, drop = FALSE])
plot(mask(elev, rmask))
plot(overlay(elev, rmask, fun = "max"))


#
# 4.2.2 Map Algebra ------------------------------

require(raster)
# What map algebra does in R?
# First, the raster package checks the headers of the rasters on which to
# perform any algebraic operation, and only if they are correspondent to each
# other, the processing goes on. Secondly, map algebra retains the so-called
# one-to-one location correspondence.

# Four subclasses:
# 1. Local or per-cell operations
# 2. Focal or neigbourhood operations
# 3. Zonal (this can take irregular shape)
# 4. Global or per-raster operations

# 4.2.3 Local operations ----------------------------

# cell-by-cell operations in one or several layers.
# For example, reclassify numeric into grouping.

# create a matrix with min, max, and factor value into 3 columns
rcl = matrix(c(0, 12, 1, 12, 24, 2, 24, 36, 3), ncol = 3, byrow = TRUE)
recl = reclassify(elev, rcl = rcl)
plot(recl)

# also direct computations
plot(elev^2)
plot(elev+elev)
plot(log(elev))
plot(elev >5)

#
# 4.2.4 Focal operations -------------------------------

# usually aggregating 3 by 3 matrix into it's center (one cell)
r_focal = focal(elev, w = matrix(1, nrow = 3, ncol = 3), fun = min)
plot(r_focal) # the outer part is gone because there are no surrounding cells which can form into 3x3 matrix


#
# 4.2.5 Zonal operations -------------------------------

# similar to Focal, but can accept irregular shape
# let's use our grain raster (3 factors)

z = zonal(elev, grain, fun = "mean") %>% as.data.frame()
z
# compute means of each factor (total = 3)

# 4.2.6 Global operations --------------------------------

# can calculate distance while also taking into account elevation value on each raster unit.

# 4.2.7 Merging rasters ------------------------------------

aut = getData("alt", country = "AUT", mask = TRUE)
ch = getData("alt", country = "CHE", mask = TRUE)
aut_ch = merge(aut, ch)

plot(aut)
plot(ch)
plot(aut_ch)

#
# 5 Geometry Operations ########################

# 5.1 Geometric operations on vector data =====================

# we will dig deeper into operations on sfc data type

# 5.1.1 Simplification ---------------------------

# you can simplify complex object to save memory / disk space

plot(seine)

seine_simp = st_simplify(seine, dTolerance = 2000) # 2000 m
plot(seine_simp)

# the higher the dTolerance, the simpler the object will be
object.size(seine)
object.size(seine_simp)


# You can also simplify polygon.
# BUT, GEOS assumes that the data is in a projected CRS and this could lead to unexpected results when using a geographic CRS.
# Therefore, we need to project the data first (eg. US National Atlas Equal Area (epsg = 2163))

us_states2163 = st_transform(us_states, 2163)
us_states_simp1 = st_simplify(us_states2163, dTolerance = 100000) # 100 km
plot(us_states_simp1[0])

# this is the downside of st_simplify, using Douglas-Peucker algoritm.

# Fix: Use Visvalingam algorithm = ms_simplify()
# The results has only 1% of the vertices of the input (argument keep) but it's number of objects remaints intact
# becauyse we set kee_shapes = TRUE

us_states2163$AREA = as.numeric(us_states2163$AREA)

require(rmapshaper)
us_states_simp2 = ms_simplify(us_states2163, keep = 0.01, keep_shapes = TRUE)
plot(us_states_simp2[0])

#

# 5.1.2 Centroids -----------------------------------------------

# centroids = center of mass in a spatial object

nz_centroid = st_centroid(nz)

# need to draw the geometry instead of the sf object
plot(nz$geom)
plot(nz_centroid$geom, col = 1, add = TRUE)

# problem with centroid is that the center of mass sometimes is outside the polygon

nz_pos = st_point_on_surface(nz)

plot(nz_pos$geom, col = 2, add = TRUE)

# other types of centroid exists: Chebyshev center, and visual center
# will not be explored in this chapter

# 5.1.3 Buffers -------------------------------------------------------------

# add buffer to shapes

seine_buff_5km = st_buffer(seine, dist = 5000)
seine_buff_50km = st_buffer(seine, dist = 50000)

plot(seine_buff_5km)
plot(seine_buff_50km)


#
# 5.1.4 Affine transformations --------------------------------------------------

# 3 types: translation, scaling, and rotation

# we need to use sfc object to do affine transformation

nz_sfc = st_geometry(nz)

## Translation
nz_shift = nz_sfc + c(0, 100000) # move all y-coordinates by 100,000 meters


## Scaling
nz_centroid_sfc = st_centroid(nz_sfc)

nz_scale = (nz_sfc - nz_centroid_sfc) * 0.5 + nz_centroid_sfc
# this code center each object into each of its center and scale it by 0.5, then add the centroid back to it's original location

plot(nz_scale)


## Rotation

# this requires a rotation matrix

# R = [cos theta   - sin theta]
#     [sin theta     cos theta]
# this matrix will rotate counter-clockwise

rotation = function(a) {
  r = a * pi / 180 # degrees to radians
  matrix(c( cos(r), sin(r), -sin(r), cos(r) ), nrow = 2, ncol = 2)
  
}

nz_rotate = (nz_sfc - nz_centroid_sfc) * rotation(90) + nz_centroid_sfc
plot(nz_rotate)

# finally, the newly created geometries can replace the old ones
nz_scale_sf = st_set_geometry(nz, nz_scale)


#
# 5.1.5 Clipping -------------------------------------------

# let's illustrate using two overlapping circles
b = st_sfc(st_point(c(0,1)), st_point(c(1,1))) # two points
b = st_buffer(b, dist = 1) # convert points to circle
plot(b)

## Intersection
x = b[1] # left circle
y = b[2] # right circle

x_and_y = st_intersection(x, y)
plot(b)
plot(x_and_y, col = "lightgrey", add = TRUE)

# different commands
plot( st_difference(y, x) ) # y not in x
plot( st_difference(x, y) ) # x not in y
plot( st_sym_difference(x, y), col = "grey" ) # x and y not intersecting with each other
plot( st_union(x, y) ) # x union y

## we can also subset points that cover the bounding box of the circles x and y

bb = st_bbox(st_union(x, y))
box = st_as_sfc(bb)

set.seed(2017)
p = st_sample(x = box, size = 10)

plot(box)
plot(x, add = TRUE)
plot(y, add = TRUE)
plot(p, add = TRUE)

sel_p_xy = st_intersects(p, x, sparse = FALSE)[,1] & st_intersects(p, y, sparse = FALSE)[, 1]

p_xy1 = p[sel_p_xy]
p_xy2 = p[x_and_y]

identical(p_xy1, p_xy2) # the clipped polygon is more consice (the second one)

#
# 5.1.6 Geometry unions -------------------------------------------

regions = aggregate(x = us_states[, "total_pop_15"], by = list(us_states$REGION),
                    FUN = sum, na.rm = TRUE)

regions2 = us_states %>%
  group_by(REGION) %>%
  summarize(pop = sum(total_pop_15, na.rm = TRUE))

plot(regions)
plot(regions2)

# you can also do st_union
us_west = us_states[us_states$REGION == "West", ]
us_west_union = st_union(us_west)
plot(us_west_union)

#
# 5.1.7 Type transformations --------------------------------------------

## cast the data from one geometric type to another
multipoint = st_multipoint(matrix(c(1, 3, 5, 1, 3, 1), ncol = 2))

linestring = st_cast(multipoint, "LINESTRING")
polyg = st_cast(multipoint, "POLYGON")
plot(multipoint)
plot(linestring)
plot(polyg)

# we can re-cast them back to points
multipoint2 = st_cast(linestring, "MULTIPOINT")
multipoint3 = st_cast(polyg, "MULTIPOINT")

all.equal(multipoint, multipoint2, multipoint3)

## now, let's cast sfc data
# some geometry types are not castable to some types (e.g. a point cannot be cast into a polygon)

multilinestring_list = list(matrix(c(1,4,5,3), ncol = 2),
                            matrix(c(4,4,4,1), ncol = 2),
                            matrix(c(2,4,2,2), ncol = 2))

multilinestring = st_multilinestring(multilinestring_list)
multilinestring_sf = st_sf(geom = st_sfc(multilinestring))
multilinestring_sf

linestring_sf2 = st_cast(multilinestring_sf, "LINESTRING")
linestring_sf2
# so we converted multiline(3) into 3 linestring


# 5.2 Geometric operations on raster data ===============================


# 5.2.1 Geometric intersections --------------------------

data("elev", package = "spData")
clip = raster(nrow = 3, ncol = 3, res = 0.3, xmn = 0.9, xmx = 1.8,
              ymn = -0.45, ymx = 0.45, vals = rep(1,9))

plot(elev)
plot(clip, add = TRUE)

plot(elev[clip, drop = FALSE])

#
# 5.2.2 Extent and origin -------------------------------------------

# meh

# 5.2.3 Aggregation and disaggregation ----------------------------------

# meh

# 5.3. Raster-vector interactions -----------------------------

# ugh

# 6 Reprojecting geographic data ##########################################

# need to specify crs manually
london = data.frame(lon = -0.1, lat = 51.5) %>%
  st_as_sf(coords = c("lon", "lat"))

st_is_longlat(london) # crs not detected

london_geo = st_set_crs(london, 4326)
st_is_longlat(london_geo) # now it's there


# st_buffer without crs data
london_buff_no_crs = st_buffer(london, dist = 1)
london_buff = st_buffer(london_geo, dist = 1)

plot(london_buff_no_crs)
plot(london_buff) # skewed, should not be like this


# st_buffer with appropriate use of crs data
london_proj = data.frame(x = 530000, y = 180000) %>%
  st_as_sf(coords = 1:2, crs = 27700)

st_crs(london_proj) # 27700 = British National Grid

# let's use a new buffer system (in meters)
london_proj_buff = st_buffer(london_proj, 111320)
plot(london_proj_buff) # now it's correct again

# 6.2 When to reproject? ======================================

st_distance(london_geo, london_proj)
# two different CRS, cannot compute distance

# transform to the same crs
london2 = st_transform(london_geo, 27700)

st_distance(london2, london_proj)

#
# 6.3 Which CRS to use? =====================================================

# no one universally accepted projection

# The most common one: WGS84
# EPSG coe: 4326

# let's create a function that calculate the EPSG code associated with any point on the planet:
lonlat2UTM = function(lonlat) {
  
  utm = (floor((lonlat[1] + 180) / 6) %% 60) + 1
  
  if(lonlat[2] > 0) {
    utm + 32600
  }
  else {
    utm + 32700
  }

}


# now, let's see the utm for Auckland and London
epsg_utm_auk = lonlat2UTM(c(174.7, -36.9))
epsg_utm_lnd = lonlat2UTM(st_coordinates(london))

st_crs(epsg_utm_auk)$proj4string
st_crs(epsg_utm_lnd)$proj4string


#
# 6.5 Modifying map projections ================================================

# custom projection

# Preserving area relationship = [Mollweide projection]
world_mollweide = st_transform(world, crs = "+proj=moll")
plot(world_mollweide[0])


# Mapping the world with little distortions to spatial properties = [Winkel triple projection]
world_wintri = lwgeom::st_transform_proj(world, crs = "+proj=wintri")
plot(world_wintri[0])


# Centered on longitude and latitude of 0 = [Lambert azimuthal equal-area projection]
world_laea1 = st_transform(world, crs = "+proj=laea +x_0=0 +y_0=0 +lon_0=0 +lat_0=0")
plot(world_laea1[0])

# we can center them at New York City if we wish to
world_laea2 = st_transform(world, crs = "+proj=laea +x_0=0 +y_0 = 0 +lon_0=-74 +lat_0=40")
plot(world_laea2$geom)

#
# 6.6 Reprojecting Raster geometries ===============================

# meh


# 7 Geographic data I/O (input/output) ####################################

# 7.2 Retrieving open data ================================================

# Standard geospatial datasets = Data.gov
# Raster datasets = GEOSS portal, Copernicus Open Access Hub
# Vector datasets = NASA, SEDAC, INSPIRE geoportal

# download using a command line!

download.file(url = "http://nrdata.nps.gov/programs/lands/nps_boundary.zip",
              destfile = "nps_boundary.zip")

unzip(zipfile = "nps_boundary.zip")
usa_parks = st_read("P:/Knowledge Management/R & A Team/Andrew A Halim/Codes stuff/R scripts/Geocomputation with R/temp/Current_Shapes/Data_Store/06-06-12_Posting/nps_boundary.shp")

plot(usa_parks$geometry)

#
# 7.3 Geographic data packages ==============================================

# command lines to quickly download geographic data:

# getlandsat = Landsat 8 data
# osmdata = download and import of OpenStreetMap data
# raster = getData() imports administrative, elevation, and WorldClim data
# rnaturalearth = access Natural Earth vector and raster data
# rnoaa = imports National Oceanic and Atmospheric Administration (NOAA) climate data
# rWBclimate = access World Bank climate data
# GSODR = provides Global Summary Daily Weather Data in R

require(rnaturalearth)
usa = ne_countries(country = "United States of America")
class(usa)

# we can convert them into sf object
usa_sf = st_as_sf(usa)

#
# 7.4 File formats ============================================================

# ugh

# 8 Making Maps with R ############################################################

require(tidyverse)
require(sf)
require(raster)
require(spData)
require(spDataLarge)

require(tmap)

require(magick) # to create animated gif

require(leaflet) # interactive maps
require(mapview) # interactive maps
require(shiny) # web application

#
# 8.2.1 tmap: basic ========================================================

require(tmap)

# basic structure: tm_shape, tm_fill, tm_borders
tm_shape(nz) +
  tm_fill() + # can remove this line if do not want to have filled shapes
  tm_borders()

# tm_polygons() << condensed fill and border into a single function
map_nz = tm_shape(nz) + tm_polygons()

# can add new stuff on top of existing map
map_nz1 = map_nz +
  tm_shape(nz_elev) + tm_raster(alpha = 0.7)

nz_water = st_union(nz) %>% st_buffer(22200) %>% # add 22200 meters buffer to NZ land region
  st_cast(to = "LINESTRING") # convert them into LINESTRING object

map_nz2 = map_nz1 +
  tm_shape(nz_water) +
  tm_lines()

map_nz3 = map_nz2 +
  tm_shape(nz_height) + # top N tallest point in NZ
  tm_dots()

tmap_arrange(map_nz1, map_nz2, map_nz3)

# 8.2.3 tmap: aesthetics =======================================================

tm_shape(nz) + tm_fill(col = "red", alpha = 0.3)
tm_shape(nz) + tm_borders(col = "blue", lwd = 3, lty = 2)

# title, and color shading
tm_shape(nz) + 
  tm_fill(col = "Land_area", title = expression("Area (km"^2*")")) +
  tm_borders()

# 8.2.4 tmap: color settings ===============================================

# add breaks manually
tm_shape(nz) +
  tm_polygons(col = "Median_income", breaks = c(0,3,4,5)*1e4 )

# specify the number of breaks
tm_shape(nz) +
  tm_polygons(col = "Median_income", n = 10 )

tm_shape(nz) +
  tm_polygons(col = "Median_income", palette = "BuGn" )

## Additional style arguments:
# 1. style = pretty | default setting, round breaks into whole numbers and spaces them evenly
# 2. style = equal  | divides input values into bins of equal range (recommended for variables with uniform distribution)
# 3. style = quantile | ensure the same number of observations fall into each category (bin size can vary widely)
# 4. style = jenks  | identify groups of similar values in the data and maximises the differences between categories
# 5. style = cont   | large number of colors over continuous field
# 6. style = cat    | categorical values and assures that each category receives a unique color

## Palette:
tm_shape(nz) + tm_polygons("Population", palette = "Blues")
tm_shape(nz) + tm_polygons("Population", palette = "YlOrBr")

#
# 8.2.5 tmap: layouts ===================================================

map_nz = tm_shape(nz) + tm_polygons("Population")

# add compass & scale bar
map_nz +
  tm_compass(type = "8star", position = c("left", "top")) +
  tm_scale_bar(breaks = c(0, 100, 200), size = 1)

map_nz +
  tm_layout(title = "New Zealand",
            scale = 3,                 # scale everything by 3x
            bg.color = "lightblue",
            frame = FALSE)


# other useful layout options:
map_nz +
  tm_layout(frame.lwd = 5)

map_nz +
  tm_layout(frame.double.line = TRUE)

map_nz +
  tm_layout(title = "uhhhhmmm but...",
            fontface = "bold",
            legend.outside = TRUE) # can also do "legend.only = TRUE" to omit the map

# color layout
map_nz +
  tm_layout(sepia.intensity = 1)

map_nz +
  tm_layout(saturation = 5)


# preset styles
map_nz +
  tm_style("bw")

map_nz +
  tm_style("classic")

map_nz +
  tm_style("cobalt")

map_nz +
  tm_style("col_blind")

#
# 8.2.6 tmap: faceted maps ===============================================


urb_1970_2030 = urban_agglomerations %>%
  filter(year %in% c(1970, 1990, 2010, 2030))

tm_shape(world) + tm_polygons() +
  tm_shape(urb_1970_2030) +
  tm_symbols(col = "black", border.col =  "white", size = "population_millions") +
  tm_facets(by = "year", nrow= 2, free.coords = FALSE)

#
# 8.2.7 tmap: inset maps ===============================================

# create a bounding box, need to know the coordinates in kilometers
nz_region = st_bbox(c(xmin = 1340000, xmax = 1450000, ymin = 5130000, ymax = 5210000),
                    crs = st_crs(nz_height)) %>%
  st_as_sfc()

# get a raster map of elevation in the bounding box region
nz_height_map = 
  tm_shape(nz_elev, bbox = nz_region) +                                  # only on the specified bounding box region
  tm_raster(style = "cont", palette = "YlGn", legend.show = TRUE) +
  tm_shape(nz_height) +
  tm_symbols(shape = 2, col ="red", size = 1) +
  tm_scale_bar(position = c("left", "bottom"))

# standard nz map
nz_map =
  tm_shape(nz) + tm_polygons() +
  tm_shape(nz_height) + tm_symbols(shape = 2, col ="red", size = 1) +
  tm_shape(nz_region) + tm_borders(lwd = 3)

# now we can combine everything
require(grid)

nz_height_map
print(nz_map, viewport(0.82, 0.30, width = 0.5, height = 0.5))


# Inset maps can be used to include regions outside geographical scope into one frame
us_states_map = tm_shape(us_states, projection = 2163) + tm_polygons() + tm_layout(frame = FALSE)

hawaii_map = tm_shape(hawaii) + tm_polygons() +
             tm_layout(title = "Hawaii", frame = FALSE, bg.color = NA, 
                       title.position = c("left", "bottom"))

alaska_map = tm_shape(alaska) + tm_polygons() + 
  tm_layout(title = "Alaska", frame = FALSE, bg.color = NA)


us_states_map
print(hawaii_map, vp = viewport(x = 0.4, y = 0.1, width = 0.2, height = 0.1))
print(alaska_map, vp = viewport(x = 0.15, y = 0.15, width = 0.3, height = 0.3))

#
# 8.3 Animated maps ===========================================================

urb_anim = tm_shape(world) +
  tm_polygons() +
  tm_shape(urban_agglomerations) +
  tm_dots(size = "population_millions") +
  tm_facets(along = "year", free.coords = FALSE)

tmap_animation(urb_anim, filename = "urb_anim.gif", delay = 25) # failed, not sure which package, but don't care

#
# 8.4 Interactive maps ======================================================

# chance mode into interactive
tmap_mode("view")
map_nz

# can specify which map server you want to use
map_nz + tm_basemap(server = "OpenTopoMap")


# we can also use tm_facets
world_coffee = left_join(world, coffee_data, by = "name_long")
facets = c("coffee_production_2016", "coffee_production_2017")

tm_shape(world_coffee) + tm_polygons(facets) +
  tm_facets(nrow = 1, sync = TRUE)

## you can set it back to static plot
tmap_mode("plot")

#
# 8.4.1 Interactive maps: Leaflet ---------------------------------------------------

require(leaflet)

pal = colorNumeric("RdYlBu", domain = cycle_hire$nbikes)

leaflet(data = cycle_hire) %>%
  addProviderTiles(providers$Stamen.TonerLite) %>%
  addCircles(col = ~pal(nbikes), opacity = 0.9) %>%
  # addPolygons(data = lnd, fill = FALSE) %>%
  addLegend(pal = pal, values = ~nbikes) %>%
  setView(lng = -0.1, 51.5, zoom = 12) %>%
  addMiniMap()

#
# 8.5 Mapping Application ======================================================

require(shiny)

ui = fluidPage(
  sliderInput(inputId =  "life", "Life expectancy", 49, 84, value = 80),
  leafletOutput(outputId = "map")
)

server = function(input, output) {
  output$map = renderLeaflet({
    leaflet() %>% addProviderTiles("OpenStreetMap.BlackandWhite") %>%
      addPolygons(data = world[world$lifeExp < input$life, ])
  })
  
}

shinyApp(ui, server) # hmmm need to find a different tutorial

#
# 11 Statistical learning ###################################




