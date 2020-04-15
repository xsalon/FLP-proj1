--- Project: DKA-2-MKA (FLP 19/20)
--- Author: Marek Salon (xsalon00)
--- Description: Implementation of minimalization algorithm

import Data.List
import Data.Maybe
import Data.Char
import Data.Function
import System.IO
import System.Environment

--------- TYPES DECLARATIONS ----------

-- custom aliases for build-in types
type State = String                     -- automata state
type Symbol = Char                      -- alphabet symbol
type Index = Int                        -- indistinguishable class index (== class id)
type Class = ([Index],[State])          -- indistinguishable class representation

-- type representing one automata transition
data Transition = Transition { 
    srcState :: State,
    tSymbol :: Symbol,
    dstState :: State
    } deriving (Show, Eq)

-- whole automata structure as type
data Automata = Automata { 
    states :: [State],
    alphabet :: [Symbol],
    startState :: State,
    endStates :: [State],
    transitions :: [Transition]
    } deriving (Eq)

-- show instance for automata to make it printable in required format
instance Show Automata where
    show (Automata states alphabet startState endStates transitions) =  
        intercalate "," states ++ "\n" ++
        alphabet ++ "\n" ++
        startState ++ "\n" ++
        intercalate "," endStates ++
        if null transitions
            then ""
            else "\n" ++ transitionsToString transitions

--------- IO OPERATIONS ----------

-- main monad function, parse input and dispatch
main :: IO ()
main = do
    (mode:path) <- getArgs
    content <- if null path
                    then getContents
                    else readFile . unwords $ path
    printResult mode (makeAutomata (lines content))

-- -i -> print parsed and validated automata from input
-- -t -> print processed automata in its minimal form (if valid)
printResult :: String -> Automata -> IO ()
printResult "-i" fsm = if isValid fsm 
                                then print fsm
                                else error "Wrong FSM syntax format!"
printResult "-t" fsm = if isValid fsm 
                                then print . minimalizeAutomata . makeCompleteAutomata . 
                                    deleteNonReachableStates $ fsm
                                else error "Wrong FSM syntax format!"
printResult _ _ = error "Usage: ./dfa-2-mka -i/-t [file]"

--------- PARSING DFA ----------

-- parse input file/string and fulfil automata structure with coresponding data 
makeAutomata :: [String] -> Automata
makeAutomata (inStates:inAlphabet:inStartState:inEndState:inTransitions) = Automata {
    states = nub . split ',' $ inStates,
    alphabet = nub inAlphabet,
    startState = nub inStartState,
    endStates = if inEndState == "" then [""] else nub . split ',' $ inEndState,
    transitions = map (makeTransition . split ',') . nub $ inTransitions
}
makeAutomata _ = error "Wrong input format!"

-- create exact transition from string format and check for bad inputs
makeTransition :: [String] -> Transition
makeTransition (s:t:d) = 
    if null t || null d
        then error "Wrong transition syntax format!"
        else 
            Transition {
                srcState = s,
                tSymbol = head t,
                dstState = head d
            }
makeTransition _ = error "Wrong transition syntax format!"

-- splits string with given character
split :: Char -> String -> [String]
split x [] = [""]
split x (c:cs)
        | c == x  = "" : rest
        | otherwise = (c : head rest) : tail rest
    where
        rest = split x cs

-- convert transition to string format
transitionsToString :: [Transition] -> String
transitionsToString xs = init (foldl strMakeOp "" xs)
    where
        strMakeOp acc x = acc ++ srcState x ++ "," ++ [tSymbol x] ++ "," ++ dstState x ++ "\n"

--------- CHECKING VALIDITY ----------

-- checking validity of automata and its components format
isValid :: Automata -> Bool
isValid fsm = okStates && okAlphabet && okStartState && okEndStates && okTransitions
    where
        okStates = all (all isDigit) (states fsm)
        okAlphabet = all (\x -> isAlpha x && isLower x) (alphabet fsm)
        okStartState = startState fsm `elem` states fsm
        okEndStates = all (\x -> x `elem` states fsm) (endStates fsm) || endStates fsm == [""]
        okTransitions = all (checkTransition fsm) (transitions fsm)

-- checking transition components for inclusion in checked sets
checkTransition :: Automata -> Transition -> Bool
checkTransition fsm trans =
    let
        srcValid = srcState trans `elem` states fsm
        tValid = tSymbol trans `elem` alphabet fsm
        dstValid = dstState trans `elem` states fsm
    in
        srcValid && tValid && dstValid

--------- DELETION OF NONREACHABLE STATES ----------

-- create DFA without nonreachable states
deleteNonReachableStates :: Automata -> Automata
deleteNonReachableStates fsm = 
    Automata {
        states = reachableStates,
        alphabet = sort . alphabet $ fsm,
        startState = startState fsm,
        endStates = endStates fsm `intersect` reachableStates,
        transitions = filter (\x -> srcState x `elem` reachableStates) (transitions fsm)
    }
    where
        reachableStates = deleteNonReachableStatesAlg fsm [startState fsm]

-- main cycle (with condition) of algorithm for deletion of nonreachable states       
deleteNonReachableStatesAlg :: Automata -> [State] -> [State]
deleteNonReachableStatesAlg fsm prevStates = 
    if prevStates == newStates
        then newStates
        else deleteNonReachableStatesAlg fsm newStates
    where
        newStates = prevStates `union` allPossibleTrans (transitions fsm) prevStates

-- for every state in final set, get destination states from possible transitions
allPossibleTrans :: [Transition] -> [State] -> [State]
allPossibleTrans [] _ = []
allPossibleTrans _ [] = []
allPossibleTrans trans (x:xs) = toDstState possibleTrans `union` allPossibleTrans trans xs
    where 
        possibleTrans = filter (\y -> x == srcState y) trans
        toDstState = foldr (\x -> union [dstState x]) []

--------- CREATING COMPLETE DFA ----------

-- create complete automata if it's not already
makeCompleteAutomata :: Automata -> Automata
makeCompleteAutomata fsm
    | isComplete fsm = fsm
    | otherwise = addSink fsm

-- checking if automata is complete (using count of transitions)
isComplete :: Automata -> Bool
isComplete fsm
    | length (transitions fsm) == length (states fsm) * length (alphabet fsm) = True
    | otherwise = False

-- add SINK (s) state and transitions to it making complete DFA
addSink :: Automata -> Automata
addSink fsm = 
    Automata {
        states = states fsm ++ ["s"],
        alphabet = alphabet fsm,
        startState = startState fsm,
        endStates = endStates fsm,
        transitions = completeTransitions
    }
    where
        completeTransitions = transitions fsm ++ getSinkTransitions fsm (states fsm ++ ["s"])

-- get all missing transitions to make automata complete
getSinkTransitions :: Automata -> [State] -> [Transition]
getSinkTransitions fsm [] = []
getSinkTransitions fsm (x:xs) = getStateComplete ++ getSinkTransitions fsm xs
    where
        symbolsWithSink = map tSymbol (filter (\y -> srcState y == x) (transitions fsm))
        getStateComplete = makeStateComplete x (alphabet fsm \\ symbolsWithSink)

-- create transitions to SINK for given state
makeStateComplete :: State -> [Symbol] -> [Transition]
makeStateComplete state = map sinkTransition
    where
        sinkTransition x = Transition {
            srcState = state,
            tSymbol = x, 
            dstState = "s"
        }

--------- MINIMALIZATION PROCESS ----------

-- starting minimalization process and initialize starting configuration
-- indistinguishable classes are represented as list of tuples [([Index],[State])]
-- first element [Index] - class signature - all destination classes reachable from current class
-- second element [State] - states of current class
-- index of class in list of classes represents class ID/name
minimalizeAutomata :: Automata -> Automata
minimalizeAutomata fsm = makeMinimalAutomata fsm (minimalizeAlg fsm initEqClass)
    where 
        initEqClass = if endStates fsm == [""]
                        then [([],states fsm)]
                        else [([],states fsm \\ endStates fsm),([],endStates fsm)]

-- main cycle of minimalization algorithm with ending condition
minimalizeAlg :: Automata -> [Class] -> [Class]
minimalizeAlg fsm prev
    | length next == length prev = makeEqClasses fsm traveledClasses traveledClasses
    | otherwise = minimalizeAlg fsm next
    where
        next = makeEqClasses fsm prev prev
        traveledClasses = travelOrder fsm (makeEqClasses fsm prev prev)

-- create list of classes
makeEqClasses :: Automata -> [Class] -> [Class] -> [Class]
makeEqClasses fsm prevClasses [] = []
makeEqClasses fsm prevClasses (x:xs) = splittedAndConnected ++ makeEqClasses fsm prevClasses xs
    where 
        splittedAndConnected = connectClasses (uncurry (splitClass fsm prevClasses) x)

-- split class into more classes if possible (if signature of states of same class is different)
splitClass :: Automata -> [Class] -> [Index] -> [State] -> [Class]
splitClass fsm prevClasses classId [] = []
splitClass fsm prevClasses classId (x:xs) = addToClass ++ splitClass fsm prevClasses classId xs
    where
        addToClass = [(makeClassSignature fsm prevClasses x, [x])]

-- make class signature - find out indexes of classes reachable from class
makeClassSignature :: Automata -> [Class] -> State -> [Index]
makeClassSignature fsm prevClasses state = determineStatesCls prevClasses sortedDstStates
    where
        filteredTrans = filter (\x -> srcState x == state) (transitions fsm)
        sortedDstStates = getDstStatesInOrder filteredTrans (alphabet fsm)

-- get destination states from transitions in order
getDstStatesInOrder :: [Transition] -> [Symbol] -> [State]
getDstStatesInOrder trans [] = []
getDstStatesInOrder trans (x:xs) = retStates : getDstStatesInOrder trans xs
    where
        transList = filter (\y -> tSymbol y == x) trans
        retStates = if null transList then [] else dstState (head transList)

-- creating list of indexes for signature
determineStatesCls :: [Class] -> [State] -> [Index]
determineStatesCls prevClasses [] = []
determineStatesCls prevClasses (x:xs) = indexVal : determineStatesCls prevClasses xs
    where
        clsIndex = findIndex (\y -> x `elem` snd y) prevClasses
        indexVal = fromMaybe (length prevClasses) clsIndex

-- connect new classes into one list, delete duplicited entries
connectClasses :: [Class] -> [Class]
connectClasses [] = []
connectClasses (x:xs) = newClass : connectClasses (filter (\y -> fst y /= fst x) xs)
    where
        newClass = (fst x, snd x ++ foldr ((++) . snd) [] (filter (\y -> fst x == fst y) xs))

-- ordering classes in required order according possible transitions from starting class
travelOrder :: Automata -> [Class] -> [Class]
travelOrder fsm classes = travelClasses filteredCls classes
    where
        filteredCls = filter (\x -> startState fsm `elem` snd x) classes

-- building new ordered list of classes/states of minimalized automata
travelClasses :: [Class] -> [Class] -> [Class]
travelClasses prev classes
    | length next >= length classes = next
    | otherwise = travelClasses next classes
    where
        next = nubBy ((==) `Data.Function.on` snd) (prev ++ addToOrdered prev)
        addToOrdered = foldr ((++) . map (\x -> classes !! x) . fst) []

--------- CREATING MKA ----------

-- creating minimal DFA from ordered indistinguishable classes
makeMinimalAutomata :: Automata -> [Class] -> Automata
makeMinimalAutomata fsm classes = Automata {
    states = map show (take (length classes) [0, 1..]),
    alphabet = alphabet fsm,
    startState = "0",
    endStates = newEndStates classes newEndClasses,
    transitions = newTrans 0 (alphabet fsm) classes
}
    where
        newEndClasses = filter (any (\ y -> y `elem` endStates fsm) . snd) classes

-- specifying new end states of automata from classes indexes
newEndStates :: [Class] -> [Class] -> [State]
newEndStates classes [] = []
newEndStates classes (x:xs) = show indexVal : newEndStates classes xs
    where
        indexVal = fromMaybe 0 (elemIndex x classes)

-- creating new list of transitions in order (for every new state)
newTrans :: Index -> [Symbol] -> [Class] -> [Transition]
newTrans cnt alpha [] = []
newTrans cnt alphas (x:xs) = makeNewTransition cnt alphas (fst x) ++ newTrans (cnt+1) alphas xs

-- for every class/state and alphabet symbol -> make new transition in order (by alphabet)
makeNewTransition :: Index -> [Symbol] -> [Index] -> [Transition]
makeNewTransition cnt _ [] = []
makeNewTransition cnt [] _ = []
makeNewTransition cnt (y:ys) (x:xs) = createNewTransition cnt y x : makeNewTransition cnt ys xs

-- creating new transition
createNewTransition :: Index -> Symbol -> Index -> Transition
createNewTransition src symb dst = Transition {
    srcState = show src,
    tSymbol = symb,
    dstState = show dst
}
