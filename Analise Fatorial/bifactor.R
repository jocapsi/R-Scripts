# Bifactor CFA

library(lavaan)

# R-code is available on the companion webpage (link in the description)

head(HolzingerSwineford1939)

?HolzingerSwineford1939

# CFA bifactor model

model_bif <- '
# factors
visual =~ x1 + x2 + x3
textual =~ x4 + x5 + x6
speed =~ x7 + x8 + x9
g_f =~ x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8 + x9

# restrictions
visual ~~ 0 * textual
visual ~~ 0 * speed
visual ~~ 0 * g_f
textual ~~ 0 * speed
textual ~~ 0 * g_f
speed ~~ 0 * g_f'

fit_bif <- cfa(model_bif,
               data = HolzingerSwineford1939)

summary(fit_bif, fit.measures =TRUE, standardized = TRUE)

# alternative way to specify uncorrelated factors

model_bif2 <- '
# Factors
visual =~ x1 + x2 + x3
textual =~ x4 + x5 + x6
speed =~ x7 + x8 + x9
g_f =~ x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8 + x9
'

fit_bif2 <- cfa(model_bif2,
                data = HolzingerSwineford1939, orthogonal = TRUE)

summary(fit_bif2, fit.measures =TRUE, standardized = TRUE)

# Attempted solution 1: Different factor identification method

model_bif2a <- '
# Factors
visual =~ x1 + x2 + x3
textual =~ x4 + x5 + x6
speed =~ x7 + x8 + x9
g_f =~ x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8 + x9
'

fit_bif2a <- cfa(model_bif2a,
                 data = HolzingerSwineford1939, orthogonal = TRUE,
                 std.lv = TRUE)

summary(fit_bif2a, fit.measures =TRUE, standardized = TRUE)

# Attempted solution 2: Starting value for loading

model_bif2b <- '
# Factors
visual =~ start(1.0) * x1 + x2 + x3
textual =~ x4 + x5 + x6
speed =~ x7 + x8 + x9
g_f =~ x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8 + x9
'

fit_bif2b <- cfa(model_bif2b,
                 data = HolzingerSwineford1939, orthogonal = TRUE,
                 std.lv = TRUE)

summary(fit_bif2b, fit.measures =TRUE, standardized = TRUE)

# Attempted solution 3: Inequality constraint

model_bif2c <- '
# Factors
visual =~ l_x1 * x1 + x2 + x3
textual =~ x4 + x5 + x6
speed =~ x7 + x8 + x9
g_f =~ x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8 + x9
l_x1 > 0
'

fit_bif2c <- cfa(model_bif2c,
                 data = HolzingerSwineford1939, orthogonal = TRUE,
                 std.lv = TRUE)

summary(fit_bif2c, fit.measures =TRUE, standardized = TRUE)

# Attempted solution 4: No subfactor "visual"

model_bif2d <- '
# Factors
textual =~ x4 + x5 + x6
speed =~ x7 + x8 + x9
g_f =~ x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8 + x9
'

fit_bif2d <- cfa(model_bif2d,
                 data = HolzingerSwineford1939, orthogonal = TRUE,
                 std.lv = TRUE)

summary(fit_bif2d, fit.measures =TRUE, standardized = TRUE)

# Attempted solution 5: x7 only on subfactor

model_bif2e <- '
# Factors
textual =~ x4 + x5 + x6
speed =~ x7 + x8 + x9
g_f =~ x1 + x2 + x3 + x4 + x5 + x6 + x8 + x9
'

fit_bif2e <- cfa(model_bif2e,
                 data = HolzingerSwineford1939, orthogonal = TRUE,
                 std.lv = TRUE)

summary(fit_bif2e, fit.measures =TRUE, standardized = TRUE)