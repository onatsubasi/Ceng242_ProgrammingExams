module PE1 where

import Text.Printf

-- PE1: Recipe Calculator
-- The premise of this homework if to write a recipe calculator that
-- calculates: how much a recipe costs to make, what can be made with the
-- ingredients already available, and how much extra ingredients need to
-- be bought in order to make a recipe.

-- Recipe = Recipe Name [(Ingredient, Quantity)]
data Recipe = Recipe String [(String, Double)] deriving Show

-- Price = Price Ingredient Quantity Price
data Price = Price String Double Double deriving Show

-- You can use this as-is
getRounded :: Double -> Double 
getRounded x = read s :: Double
               where s = printf "%.2f" x

-- Calculate how much the given amount of the given ingredient costs
getIngredientCost :: (String, Double) -> [Price] -> Double

getIngredientCost (ingname1, gram1) (Price ingname2 gram2 price : lst) = if ingname1 == ingname2 then
                getRounded((gram1/gram2)*price) else getIngredientCost (ingname1, gram1) lst            

-- Calculate how much it costs to buy all the ingredients of a recipe
recipeCost :: Recipe -> [Price] -> Double
recipecost (Recipe recipename []) _ = 0
recipeCost (Recipe recipename (item:lst)) prices = 
    getIngredientCost item prices + recipeCost (Recipe recipename lst) prices
recipeCost _ _ = 0
-- Given a list of how much you already have of each ingredient,
-- calculate how much of which ingredients are missing for a recipe



missingIngredients :: Recipe -> [(String, Double)] -> [(String, Double)]

missingIngredients (Recipe recipename lst) stock = [(ingname,ingAmount - stockAmount) | (ingname,ingAmount) <-lst, (ingname1,stockAmount) <-stock, ingname == ingname1,
 ingAmount - stockAmount > 0] ++ [(ingname,ingAmount) | (ingname,ingAmount) <- lst, notElem ingname  [fst x | x <- stock] ] 

-- Given a list of ingredients in your kitchen, calculate what you would
-- have left after making the given recipe. If there isn't enough of an
-- ingredient, the recipe cannot be made! You shouldn't change the amount
-- of ingredient in that case.
checkRecipe :: [(String, Double)] -> Recipe -> Bool
checkRecipe stock (Recipe recipename lst) = null [stockAmount - recipeAmount | (itemname,stockAmount) <- stock, (itemname1,recipeAmount) <- lst, stockAmount -recipeAmount < 0]

makeRecipeH ::[(String, Double)] -> Recipe -> Bool -> [(String, Double)]
makeRecipeH stock _ False = stock
makeRecipeH stock (Recipe recipename lst) True = [(itemname, stockAmount - recipeAmount) | (itemname,stockAmount) <- stock, (itemname1,recipeAmount) <- lst,
                                                                                 itemname == itemname1] ++
                                                                                 [(itemname,stockAmount) | (itemname,stockAmount) <- stock, notElem itemname  [fst x | x <- lst] ]

makeRecipe :: [(String, Double)] -> Recipe -> [(String, Double)]
makeRecipe stock (Recipe recipename lst) = makeRecipeH stock (Recipe recipename lst) (checkRecipe stock (Recipe recipename lst))



-- Given a list of ingredients you already have, and a list of recipes,
-- make a shopping list showing how much of each ingredient you need
-- to buy, and its cost. Each ingredient mush appear in the shopping list
-- at most once (no duplicates!).
flattenRecipe :: [Recipe] ->[(String,Double)] -> [(String,Double)]
flattenRecipe ((Recipe name lst):list) []=  flattenRecipe list lst
flattenRecipe ((Recipe name lst):list) finallist = flattenRecipe list finallist_r
    where finallist_r = [(itemname1, itemAmount + extraAmount) | (itemname1,itemAmount) <- lst, (itemname2,extraAmount) <- finallist, itemname1 == itemname2] ++
            [(item, amount) | (item,amount) <- lst, notElem item [fst x| x <- finallist ]] ++
            [(f_item, f_amount) | (f_item, f_amount) <- finallist, notElem f_item [fst x| x <- lst]]
flattenRecipe [] finallist  = finallist



makeShoppingListH :: [(String, Double)] -> [(String,Double)] -> [Price] -> [(String, Double, Double)]
makeShoppingListH stock ings prices = [(ingName, amount, getIngredientCost (ingName,amount) prices) | (ingName, amount) <- ings , notElem ingName [fst x | x <- stock]] ++
                                        [(ingName, amount - amount1, getIngredientCost (ingName,amount-amount1) prices) | (ingName1, amount) <- ings, (ingName,amount1) <- stock,ingName == ingName1, amount - amount1 >0 ]

makeShoppingList :: [(String, Double)] -> [Recipe] -> [Price] -> [(String, Double, Double)]
makeShoppingList stock recipelist prices = makeShoppingListH stock ings prices where ings = flattenRecipe recipelist []




