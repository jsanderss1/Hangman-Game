# Hangman-Game
This is a game designed to run on top of Racket through the console. That way you will avoid some of the input/output procedures regarding looping interaction with a user. It will be run as a Scheme program,  and the persistent state of the program will be simulated with global symbols that can be reassigned. Therefore, parts of the code will not follow pure Functional Programming. The list of the words to be guessed will be also loaded from an external file present in the same directory as the  game. The system will randomly select a word among this list when the player starts a game. You are free to implement the functions using either recursion or high-order programming functionals (map, filter,  foldr, foldl, apply). Bear in mind you should process strings as list of chars. In case you have to write some  complex level-nested expressions, consider using (let, let*) to float sub-expressions locally to gain readability
