module Spec where
import PdePreludat
import Library
import Test.Hspec
import Control.Exception (evaluate)

correrTests :: IO ()
correrTests = hspec $ do

    describe "TP 5" $ do

        describe "Parte I" $ do

            it "agrandar una hamburgesa con carne debe retornar la hamburguesa con una carne mas" $ do
                agrandar (Hamburguesa 0 [Carne]) `shouldBe` Hamburguesa 0 [Carne, Carne]
            it "agrandar una hamburgesa con pollo debe retornar la hamburguesa con un pollo mas" $ do
                agrandar (Hamburguesa 0 [Pollo]) `shouldBe` Hamburguesa 0 [Pollo, Pollo]
            it "agrandar una hamburgesa con pollo y carne debe retornar la hamburguesa con algunos de esos dos mas" $ do
                agrandar (Hamburguesa 0 [Pollo, Carne]) `shouldBe` Hamburguesa 0 [Carne, Pollo, Carne]

            it "descuento deberia aplicarle un descuento al precioBase de la hamburgesa" $ do
                precioBase (descuento 50 cuartoDeLibra) `shouldBe` 10

            it "la pdepBurger deberia ser un cuarto de libra agrandado 2 veces con panceta, cheddar y 20% de descuento" $ do
                pdepBurger `shouldBe` Hamburguesa 16 [Panceta, Cheddar, Carne, Carne, Pan, Carne, Cheddar, Pan]
            it "el precio final de la pdepBurger deberia ser 110" $ do
                precioFinal pdepBurger `shouldBe` 110



        describe "Parte II" $ do

            it "el dobleCuarto deberia ser un cuarto de libra con carne y cheddar" $ do
                dobleCuarto `shouldBe` Hamburguesa 20 [Cheddar, Carne, Pan, Carne, Cheddar, Pan]
            it "el precio final del dobleCuarto deberia ser 84" $ do
                precioFinal dobleCuarto `shouldBe` 84

            it "el bigPdep deberia ser un dobleCuarto con curry" $ do
                bigPdep `shouldBe` Hamburguesa 20 [Curry, Cheddar, Carne, Pan, Carne, Cheddar, Pan]
            it "el precio final del bigPdep deberia ser 89" $ do
                precioFinal bigPdep `shouldBe` 89

            it "el delDia deberia agregarle papas a una hamburguesa con un 30% de descuento" $ do
                delDia cuartoDeLibra `shouldBe` descuento 30 (Hamburguesa 20 [Papas, Pan, Carne, Cheddar, Pan])
            it "el precio final del dobleCuarto delDia deberia ser 88" $ do
                precioFinal (delDia dobleCuarto) `shouldBe` 88



        describe "Parte III" $ do

            it "hacerVeggie debe cambiar los ingredientes base por PatiVegano, el Cheddar por QuesoDeAlmendras y la Panceta por BaconDeTofu" $ do
                hacerVeggie (Hamburguesa 20 [Carne, Pollo, Panceta, Papas, Cheddar]) `shouldBe` Hamburguesa 20 [PatiVegano, PatiVegano, BaconDeTofu, Papas, QuesoDeAlmendras]
            it "agrandar una hamburgesa veggie debe agregarle un PatiVegano" $ do
                agrandar (hacerVeggie cuartoDeLibra) `shouldBe` Hamburguesa 20 [PatiVegano, Pan, PatiVegano, QuesoDeAlmendras, Pan]

            it "cambiarPanDePati debe hacer eso, cambiar Pan por PanIntegral" $ do
                cambiarPanDePati (Hamburguesa 20 [Pan, Carne, Pan]) `shouldBe` Hamburguesa 20 [PanIntegral, Carne, PanIntegral]

            it "el dobleCuartoVegano debe ser un dobleCuarto veggie con PanIntegral" $ do
                dobleCuartoVegano `shouldBe` Hamburguesa 20 [QuesoDeAlmendras, PatiVegano, PanIntegral, PatiVegano, QuesoDeAlmendras, PanIntegral]