module Library where
import PdePreludat

data Ingrediente =
    Carne | Pan | Panceta | Cheddar | Pollo | Curry | QuesoDeAlmendras | BaconDeTofu | Papas | PatiVegano | PanIntegral
    deriving (Eq, Show)

precioIngrediente :: Ingrediente -> Number
precioIngrediente Carne = 20
precioIngrediente Pan = 2
precioIngrediente Panceta = 10
precioIngrediente Cheddar = 10
precioIngrediente Pollo =  10
precioIngrediente Curry = 5
precioIngrediente QuesoDeAlmendras = 15
precioIngrediente BaconDeTofu = 12
precioIngrediente Papas = 10
precioIngrediente PatiVegano = 10
precioIngrediente PanIntegral = 3

data Hamburguesa = Hamburguesa {
    precioBase :: Number,
    ingredientes :: [Ingrediente]
} deriving (Eq, Show)

cuartoDeLibra :: Hamburguesa
cuartoDeLibra = Hamburguesa {precioBase = 20, ingredientes = [Pan, Carne, Cheddar, Pan]}



-------------------- PARTE 1 --------------------

ingredientesBase :: [Ingrediente]
ingredientesBase = [Carne, Pollo, PatiVegano]

agregarIngrediente :: Ingrediente -> Hamburguesa -> Hamburguesa
agregarIngrediente ing burger = burger {ingredientes = ing:ingredientes burger}

agrandar :: Hamburguesa -> Hamburguesa
agrandar burger -- podría hacerse mejor
    | elem Carne (ingredientes burger) = agregarIngrediente Carne burger
    | elem Pollo (ingredientes burger) = agregarIngrediente Pollo burger
    | otherwise = agregarIngrediente PatiVegano burger

descuento :: Number -> Hamburguesa -> Hamburguesa
descuento porcentaje burger = burger {precioBase = (1 - porcentaje/100) * precioBase burger}


pdepBurger :: Hamburguesa
pdepBurger = descuento 20 . agregarIngrediente Panceta . agregarIngrediente Cheddar . agrandar . agrandar $ cuartoDeLibra


precioIngredientes :: Hamburguesa -> Number
precioIngredientes burger = sum . map (precioIngrediente) . ingredientes $ burger

precioFinal :: Hamburguesa -> Number
precioFinal burger = precioBase burger + precioIngredientes burger



-------------------- PARTE 2 --------------------

dobleCuarto :: Hamburguesa
dobleCuarto = agregarIngrediente Cheddar . agrandar $ cuartoDeLibra

bigPdep :: Hamburguesa
bigPdep = agregarIngrediente Curry dobleCuarto

delDia :: Hamburguesa -> Hamburguesa
delDia burger = descuento 30 . agregarIngrediente Papas $ burger



-------------------- PARTE 3 --------------------

swapVeggie :: Ingrediente -> Ingrediente
swapVeggie Pollo = PatiVegano
swapVeggie Carne = PatiVegano
swapVeggie Cheddar = QuesoDeAlmendras
swapVeggie Panceta = BaconDeTofu
swapVeggie ing = ing

swapIntegral :: Ingrediente -> Ingrediente
swapIntegral Pan = PanIntegral
swapIntegral ing = ing

mapIngredientes :: (Ingrediente -> Ingrediente) -> Hamburguesa -> Hamburguesa
mapIngredientes f burger = burger {ingredientes = map f . ingredientes $ burger}

hacerVeggie :: Hamburguesa -> Hamburguesa
hacerVeggie = mapIngredientes swapVeggie

cambiarPanDePati :: Hamburguesa -> Hamburguesa
cambiarPanDePati = mapIngredientes swapIntegral

dobleCuartoVegano :: Hamburguesa
dobleCuartoVegano = cambiarPanDePati . hacerVeggie $ dobleCuarto
