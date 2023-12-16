module Commands.Instructions where

-- Function to display available instructions
displayInstructions :: IO ()
displayInstructions = do
  putStrLn "Available Instructions:"
  putStrLn "  - look: View the current room"
  putStrLn "  - inspect X: Inspect an object in the room"
  putStrLn "  - take X: Take an item from the room and add it to your inventory"
  putStrLn "  - inventory: View your current inventory"
  putStrLn "  - hint X: View a hint for an object or `hint here` for the current room if you are stuck"
  putStrLn "  - go DIRECTION: move to another room to the north, south, east or west"
  putStrLn "  - unlock DIRECTION: unlock doors in this direction"
  putStrLn "  - enter X: to go inside X"
  putStrLn "  - instructions: Display this list again"
  putStrLn "... and more for you to find out on your own (or by reading hints)\n"
