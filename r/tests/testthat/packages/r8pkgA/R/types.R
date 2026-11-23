# types.R
# ::r8pkgA::
# 2026- EDG rtemis.org

# Declarations at the top level run when the package is installed; .onLoad()
# registers them in the session.

Orientation <- R8::new_enum(
  "Orientation",
  variants = list(Wide = R8::variant(), Tall = R8::variant())
)

Rect <- R8::new_struct(
  "Rect",
  fields = list(
    w = R8::type_double(min = 0),
    h = R8::type_double(min = 0),
    orientation = R8::field(Orientation, default = Orientation@Wide)
  ),
  rules = list(R8::rule(
    w >= h,
    when = is_variant(orientation, "Wide"),
    message = "A wide rectangle is at least as wide as it is tall.",
    id = "WIDE"
  ))
)

Area <- R8::new_trait(
  "Area",
  methods = list(area = R8::trait_method(function(x, ...) NULL))
)
area <- Area@area

R8::impl(Area, Rect, methods = list(area = function(x, ...) x$w * x$h))
