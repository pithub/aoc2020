interface Day21 exposes [ output ] imports [ ListTree, TestUtil ]


output : List I64 -> List (List I64)
output = \puzzleInput ->
    testData   = parseData testInput
    puzzleData = parseData puzzleInput

    [ TestUtil.verify 21 1 1 (firstResult testData  ) 5
    , TestUtil.show   21 1   (firstResult puzzleData)
    , List.concat [ 9, 21, 2, 1 ] (secondResult testData  )
    , List.concat [ 9, 21, 2, 0 ] (secondResult puzzleData)
    ]


#  first part


firstResult : List I64 -> I64
firstResult = \data ->
    allergenes = getAllergenes data
    ingredients =
        getIngredients allergenes data
            |> removeAllergenes allergenes

    ingPtr = ListTree.firstP ingredients
    allPosMax = ListTree.size allergenes + 4
    firstResultHelper ingredients ingPtr allPosMax 4 1 0


firstResultHelper : List I64, I64, I64, I64, I64, I64 -> I64
firstResultHelper = \ingredients, ingPtr, allPosMax, allPos, resultIncrement, result ->
    if allPos < allPosMax then
        if ListTree.getP ingredients ingPtr allPos > 0 then
            firstResultHelper ingredients ingPtr allPosMax allPosMax 0 result
        else
            firstResultHelper ingredients ingPtr allPosMax (allPos + 1) resultIncrement result
    else
        newIngPtr = ListTree.nextP ingredients ingPtr
        newResult = result + resultIncrement * ListTree.getP ingredients ingPtr 1
        if newIngPtr > 0 then
            firstResultHelper ingredients newIngPtr allPosMax 4 1 newResult
        else
            newResult


#  second part


#  Test:
#  ---------
#  a 1 = 1.1 dairy
#  a 2 = 1.2 fish
#  a 3 = 3.1 soy
#  ---------
#  i 1 = 1.1 -> a 1 mxmxvkd
#  i 3 = 1.3 -> a 2 sqjhc
#  i 6 = 2.2 -> a 3 fvjkl

#  Puzzle:
#  ---------
#  a 3 = 2.3 dairy
#  a 4 = 3.1 eggs
#  a 7 = 5.1 fish
#  a 2 = 2.2 nuts
#  a 1 = 1.1 peanuts
#  a 5 = 4.2 sesame
#  a 8 = 7.2 soy
#  a 6 = 4.3 wheat
#  ---------
#  i  11 = 1.11 -> 3 vmhqr
#  i  64 = 1.64 -> 4 qxfzc
#  i  76 = 1.76 -> 7 khpdjv
#  i   8 = 1. 8 -> 2 gnrpml
#  i  24 = 1.24 -> 1 xrmxxvn
#  i 109 = 3.13 -> 5 rfmvh
#  i  63 = 1.63 -> 8 rdfr
#  i  67 = 1.67 -> 6 jxh

# vmhqr,qxfzc,khpdjv,gnrpml,xrmxxvn,rfmvh,rdfr,jxh


secondResult : List I64 -> List I64
secondResult = \data ->
    allergenes = getAllergenes data
    ingredients =
        getIngredients allergenes data
            |> removeAllergenes allergenes

    allPtr = ListTree.firstP allergenes
    allergenNums = getAllergenNums allergenes allPtr []

    ingPtr = ListTree.firstP ingredients
    allPosMax = ListTree.size allergenes + 4
    secondResultHelper ingredients ingPtr allPosMax 4 allergenNums


getAllergenNums : List I64, I64, List I64 -> List I64
getAllergenNums = \allergenes, allPtr, result ->
    if allPtr > 0 then
        newAllPtr = ListTree.nextP allergenes allPtr
        newResult = List.concat result
            [ ListTree.getP allergenes allPtr 0
            , ListTree.getP allergenes allPtr 2
            , ListTree.getP allergenes allPtr 3
            ]
        getAllergenNums allergenes newAllPtr newResult
    else
        result


secondResultHelper : List I64, I64, I64, I64, List I64 -> List I64
secondResultHelper = \ingredients, ingPtr, allPosMax, allPos, result ->
    if allPos < allPosMax then
        if ListTree.getP ingredients ingPtr allPos > 0 then
            newResult = List.concat result
                [ ListTree.getP ingredients ingPtr 0
                , ListTree.getP ingredients ingPtr 2
                , ListTree.getP ingredients ingPtr 3
                , allPos - 3
                ]
            secondResultHelper ingredients ingPtr allPosMax allPosMax newResult
        else
            secondResultHelper ingredients ingPtr allPosMax (allPos + 1) result
    else
        newIngPtr = ListTree.nextP ingredients ingPtr
        if newIngPtr > 0 then
            secondResultHelper ingredients newIngPtr allPosMax 4 result
        else
            result


#  util


getAllergenes : List I64 -> List I64
getAllergenes = \data ->
    foodCnt = getSafe data 0
    dataPos = getSafe data 1 + 3
    allCnt  = getSafe data 2

    ListTree.empty 1 4
        |> getAllergenesHelper data dataPos foodCnt 1 allCnt 0


getAllergenesHelper : List I64, List I64, I64, I64, I64, I64, I64 -> List I64
getAllergenesHelper = \allergenes, data, dataPos, foodCnt, foodNum, allCnt, allIdx ->
    if allIdx < allCnt then
        newDataPos = dataPos + 1
        newAllIdx = allIdx + 1

        key = getSafe data dataPos
        newAllergenes = ListTree.insertP allergenes key
        
        if ListTree.inserted newAllergenes then
            allNum = ListTree.size newAllergenes
            foodNumHash = addChar 5301 foodNum
            newAllergenes
                |> ListTree.setI 0 allNum
                |> ListTree.setI 1 foodNumHash
                |> ListTree.setI 2 foodNum
                |> ListTree.setI 3 newAllIdx
                |> getAllergenesHelper data newDataPos foodCnt foodNum allCnt newAllIdx

        else
            foodNumHash =
                newAllergenes
                    |> ListTree.getI 1
                    |> addChar foodNum
            newAllergenes
                |> ListTree.setI 1 foodNumHash
                |> getAllergenesHelper data newDataPos foodCnt foodNum allCnt newAllIdx

    else if foodNum < foodCnt then
        newFoodNum = foodNum + 1
        newDataPos = getSafe data (dataPos + 0) + dataPos + 2
        newAllCnt  = getSafe data (dataPos + 1)
        allergenes
            |> getAllergenesHelper data newDataPos foodCnt newFoodNum newAllCnt 0
    
    else
        allergenes


getIngredients : List I64, List I64 -> List I64
getIngredients = \allergenes, data ->
    foodCnt = getSafe data 0
    ingCnt  = getSafe data 1
    allCnt  = getSafe data 2
    dataPos = 3

    allTotal = ListTree.size allergenes
    ListTree.empty 1 (4 + allTotal)
        |> getIngredientsHelper allergenes allCnt data dataPos foodCnt 1 ingCnt 0


getIngredientsHelper : List I64, List I64, I64, List I64, I64, I64, I64, I64, I64 -> List I64
getIngredientsHelper = \ingredients, allergenes, allCnt, data, dataPos, foodCnt, foodNum, ingCnt, ingIdx ->
    if ingIdx < ingCnt then
        newDataPos = dataPos + 1
        newIngIdx = ingIdx + 1

        allPos = dataPos + ingCnt - ingIdx

        key = getSafe data dataPos
        newIngredients = ListTree.insertP ingredients key
        
        if ListTree.inserted newIngredients then
            ingNum = ListTree.size newIngredients
            newIngredients
                |> ListTree.setI 0 ingNum
                |> ListTree.setI 1 1
                |> ListTree.setI 2 foodNum
                |> ListTree.setI 3 newIngIdx
                |> addAllergenes allergenes allCnt data foodNum allPos 0
                |> getIngredientsHelper allergenes allCnt data newDataPos foodCnt foodNum ingCnt newIngIdx

        else
            ingFoodCnt = ListTree.getI newIngredients 1
            newIngredients
                |> ListTree.setI 1 (ingFoodCnt + 1)
                |> addAllergenes allergenes allCnt data foodNum allPos 0
                |> getIngredientsHelper allergenes allCnt data newDataPos foodCnt foodNum ingCnt newIngIdx

    else if foodNum < foodCnt then
        newFoodNum = foodNum + 1
        newDataPos = dataPos + allCnt + 2
        newIngCnt  = getSafe data (newDataPos - 2)
        newAllCnt  = getSafe data (newDataPos - 1)

        ingredients
            |> getIngredientsHelper allergenes newAllCnt data newDataPos foodCnt newFoodNum newIngCnt 0
    
    else
        ingredients


addAllergenes : List I64, List I64, I64, List I64, I64, I64, I64 -> List I64
addAllergenes = \ingredients, allergenes, allCnt, data, foodNum, dataPos, allIdx ->
    if allIdx < allCnt then
        newDataPos = dataPos + 1
        newAllIdx = allIdx + 1

        allHash = getSafe data dataPos
        when ListTree.get allergenes allHash is
            Ok allNum ->
                foodNumPos = allNum + 3
                foodNumHash = ListTree.getI ingredients foodNumPos
                oldFoodNumHash = if foodNumHash == 0 then 5301 else foodNumHash
                newFoodNumHash = addChar oldFoodNumHash foodNum

                ingredients
                    |> ListTree.setI foodNumPos newFoodNumHash
                    |> addAllergenes allergenes allCnt data foodNum newDataPos newAllIdx

            _ ->
                ingredients
                    |> addAllergenes allergenes allCnt data foodNum newDataPos newAllIdx

    else
        ingredients


removeAllergenes : List I64, List I64 -> List I64
removeAllergenes = \ingredients, allergenes ->
    ingPtr = ListTree.firstP ingredients
    allPtr = ListTree.firstP allergenes
    removeAllergenesHelper ingredients ingPtr allergenes allPtr


removeAllergenesHelper : List I64, I64, List I64, I64 -> List I64
removeAllergenesHelper = \ingredients, ingPtr, allergenes, allPtr ->
    if allPtr > 0 then
        newAllPtr = ListTree.nextP allergenes allPtr

        allNum = ListTree.getP allergenes allPtr 0
        foodNumPos = allNum + 3
        foodNumHash = ListTree.getP ingredients ingPtr foodNumPos
        if foodNumHash > 0 && foodNumHash != ListTree.getP allergenes allPtr 1 then
            ingredients
                |> removeAllergene ingPtr allergenes foodNumPos
                |> removeAllergenesHelper ingPtr allergenes newAllPtr
        else
            ingredients
                |> removeAllergenesHelper ingPtr allergenes newAllPtr

    else
        newIngPtr = ListTree.nextP ingredients ingPtr
        if newIngPtr > 0 then
            newAllPtr = ListTree.firstP allergenes
            ingredients
                |> removeAllergenesHelper newIngPtr allergenes newAllPtr
        else
            ingredients


removeAllergene : List I64, I64, List I64, I64 -> List I64
removeAllergene = \ingredients, ingPtr, allergenes, foodNumPos ->
    ingredients
        |> ListTree.setP ingPtr foodNumPos 0
        |> handleIdentifiedAllergenes allergenes foodNumPos


handleIdentifiedAllergenes : List I64, List I64, I64 -> List I64
handleIdentifiedAllergenes = \ingredients, allergenes, foodNumPos ->
    ingPtr = getIngredientForAllergene ingredients (ListTree.firstP ingredients) foodNumPos 0
    if ingPtr > 0 then
        allPosMax = ListTree.size allergenes + 4
        removeOtherAllergenes ingredients ingPtr allergenes allPosMax foodNumPos 4
    else
        ingredients


getIngredientForAllergene : List I64, I64, I64, I64 -> I64
getIngredientForAllergene = \ingredients, ingPtr, foodNumPos, result ->
    if ingPtr > 0 then
        newIngPtr = ListTree.nextP ingredients ingPtr
        if ListTree.getP ingredients ingPtr foodNumPos > 0 then
            if result > 0 then
                0
            else
                getIngredientForAllergene ingredients newIngPtr foodNumPos (ingPtr + 0)
        else
            getIngredientForAllergene ingredients newIngPtr foodNumPos result
    else
        result


removeOtherAllergenes : List I64, I64, List I64, I64, I64, I64 -> List I64
removeOtherAllergenes = \ingredients, ingPtr, allergenes, allPosMax, foodNumPos, allPos ->
    if allPos > allPosMax then
        ingredients
    else
        newAllPos = allPos + 1
        if allPos == foodNumPos || ListTree.getP ingredients ingPtr allPos == 0 then
            ingredients
                |> removeOtherAllergenes ingPtr allergenes allPosMax foodNumPos newAllPos
        else
            ingredients
                |> removeAllergene ingPtr allergenes allPos
                |> removeOtherAllergenes ingPtr allergenes allPosMax foodNumPos newAllPos


getSafe : List I64, I64 -> I64
getSafe = \list, idx ->
    when List.get list idx is
        Ok val -> val
        _ -> 0


#  parser


parseData : List I64 -> List I64
parseData = \input ->
    input
        |> parseDataPass1 0 0 []
        |> parseDataPass2 0 1 1 0 [ 0, 0, 0 ]


parseDataPass1 : List I64, I64, I64, List I64 -> List I64
parseDataPass1 = \input, pos, hash, result ->
    when List.get input pos is
        Ok val ->
            if val < 65 then
                newResult1 =
                    if hash > 0 then
                        List.append result hash
                    else
                        result

                newPos =
                    if val == 40 then
                        pos + 10
                    else if val == 44 then
                        pos + 2
                    else
                        pos + 1

                newResult2 =
                    if val == 40 || val == 41 then
                        List.append newResult1 -1
                    else if val == 10 then
                        List.append newResult1 0
                    else
                        newResult1

                parseDataPass1 input newPos 0 newResult2

            else
                newHash =
                    if hash > 0 then
                        addChar hash val
                    else
                        addChar 5301 val

                parseDataPass1 input (pos + 1) newHash result

        _ ->
            result


parseDataPass2 : List I64, I64, I64, I64, I64, List I64 -> List I64
parseDataPass2 = \input, pos, start, foodNum, hashNum, result ->
    when List.get input pos is
        Ok val ->
            if val == 0 then
                newStart = List.len result
                newResult = List.concat result [ 0, 0 ]
                parseDataPass2 input (pos + 1) newStart (foodNum + 1) 0 newResult
            else if val < 0 then
                newResult = List.set result start hashNum
                parseDataPass2 input (pos + 1) (start + 1) foodNum 0 newResult
            else
                newResult = List.append result val
                parseDataPass2 input (pos + 1) start foodNum (hashNum + 1) newResult
        _ ->
            List.set result 0 foodNum


addChar = \hash, char ->
    when (hash * 33 + char) % 144115188075855871 is
        Ok newHash ->
            newHash
        _ ->
            5301


#  test data


testInput : List I64
testInput =
    [ 109, 120, 109, 120, 118, 107, 100, 32
    , 107, 102, 99, 100, 115, 32
    , 115, 113, 106, 104, 99, 32
    , 110, 104, 109, 115, 32
    , 40, 99, 111, 110, 116, 97, 105, 110, 115, 32
    , 100, 97, 105, 114, 121, 44, 32
    , 102, 105, 115, 104, 41
    , 10
    , 116, 114, 104, 32
    , 102, 118, 106, 107, 108, 32
    , 115, 98, 122, 122, 102, 32
    , 109, 120, 109, 120, 118, 107, 100, 32
    , 40, 99, 111, 110, 116, 97, 105, 110, 115, 32
    , 100, 97, 105, 114, 121, 41
    , 10
    , 115, 113, 106, 104, 99, 32
    , 102, 118, 106, 107, 108, 32
    , 40, 99, 111, 110, 116, 97, 105, 110, 115, 32
    , 115, 111, 121, 41
    , 10
    , 115, 113, 106, 104, 99, 32
    , 109, 120, 109, 120, 118, 107, 100, 32
    , 115, 98, 122, 122, 102, 32
    , 40, 99, 111, 110, 116, 97, 105, 110, 115, 32
    , 102, 105, 115, 104, 41
    ]
