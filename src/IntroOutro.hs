module IntroOutro where

introduction :: String
introduction = "\nAs you slowly open your eyes, a throbbing ache reverberates through your head,\n" ++
    "and the acrid scent of chemicals fills the air. Disoriented, you find yourself lying\n" ++
    "on a cold metal floor in what appears to be a dimly lit locker room. The flickering\n" ++
    "light above casts unsettling shadows on the lockers that surround you.\n" ++
    "\n" ++
    "A soft hum of emergency alarms pulsates through the room, and the distant echoes\n" ++
    "of chaotic commotion resonate in the air. Struggling to your feet, you notice a faint\n" ++
    "glow emanating from a small screen hanged on the wall near the entrance.\n" ++
    "The screen flickers to life, displaying a message in stark white letters:\n" ++
    "\n" ++
    "\"Dr S., the experiment has failed. Containment breach detected. Evacuation\n" ++
    "protocol initiated. Escape the facility before critical systems fail. Beware of\n" ++
    "anomalies. Your survival is paramount. Time is of the essence.\"\n" ++
    "\n" ++
    "With a sense of urgency, you realize that your fate is entwined with the impending\n" ++
    "collapse of the lab. The clock is ticking, and the path to freedom is uncertain.\n\n"


outro :: String
outro = "\nAs you ascend through the elevator, leaving the depths of the lab behind, a sense\n" ++
    "of accomplishment washes over you. The familiar hum of the machinery gradually\n" ++
    "fades as you reach the surface, greeted by natural light and the open air. The\n" ++
    "door opens, revealing a world beyond the confines of the secretive lab. You have\n" ++
    "successfully escaped, leaving the mysteries and experiments behind. What lies\n" ++
    "ahead is a new chapter, and the journey to freedom is now yours to explore.\n\n"

displayIntro :: IO ()
displayIntro = putStr(introduction)

displayOutro :: IO ()
displayOutro = putStr(outro)