module Library where
import PdePreludat

data Ingrediente =
    Carne | Pan | Panceta | Cheddar | Pollo | Curry | QuesoDeAlmendras
    deriving (Eq, Show)

precioIngrediente :: Ingrediente -> Number
precioIngrediente Carne = 20
precioIngrediente Pan = 2
precioIngrediente Panceta = 10
precioIngrediente Cheddar = 10
precioIngrediente Pollo =  10
precioIngrediente Curry = 5
precioIngrediente QuesoDeAlmendras = 15

data Hamburguesa = Hamburguesa {
    precioBase :: Number,
    ingredientes :: [Ingrediente]
} deriving (Eq, Show)

cuartoDeLibra :: Hamburguesa
cuartoDeLibra = Hamburguesa {precioBase = 20, ingredientes = [Pan, Carne, Cheddar, Pan]}


-------------------- PARTE 1 --------------------

agregarIngrediente :: Ingrediente -> Hamburguesa -> Hamburguesa
agregarIngrediente ing burger = burger {ingredientes = ing:ingredientes burger}

agrandar :: Hamburguesa -> Hamburguesa
agrandar burger
    | elem Carne (ingredientes burger) = agregarIngrediente Carne burger
    | otherwise = agregarIngrediente Pollo burger

descuento :: Number -> Hamburguesa -> Hamburguesa
descuento porcentaje burger = burger {precioBase = (1 - porcentaje/100) * precioBase burger}

pdepBurger :: Hamburguesa
pdepBurger = descuento 20 . agregarIngrediente Panceta . agregarIngrediente Cheddar . agrandar . agrandar $ cuartoDeLibra
-- >>> pdepBurger
-- Hamburguesa {precioBase = 16, ingredientes = [Panceta,Cheddar,Carne,Carne,Pan,Carne,Cheddar,Pan]}

{- descuento: recibe un % de descuento, y devuelve la hamburguesa con ese descuento aplicado al precio base.
la pdepBurger, que es un cuarto de libra agrandado 2 veces con panceta, cheddar y 20% de descuento. Su precio final deberia ser 110. -}
{- agrandar: cada vez que se agranda una hamburguesa se agrega otro ingrediente base (por ahora, son Carne o Pollo), se elige el ingrediente base a agregar según lo que ya haya en la hamburguesa (si había carne se agrega carne, si había pollo se agrega pollo, si había ambos da igual cuál se agregue). -}
