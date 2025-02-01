## Message encryption with Haskell

### Overview
The project consists of several functions that encrypt the message provided by the user, using the Caesar cipher or the Vigenère cipher, in accordance with the [documentation](Documentation.pdf). The documentation is written in Spanish.

### Challenges and insights
This was a collaborative project where I took the lead on coding, while my teammates focused on testing. To design the system, I broke down the problem into several recursive functions, adhering to the principles of functional programming. I have a strong passion for recursion, which made this project particularly enjoyable for me. I programmed simple functions from scratch to build a more complex solution, such as implementing a message encryption system. This experience allowed me to deepen my understanding of functional programming and strengthen my problem-solving skills.

### How to run
- The test file is located within the project. You can use it to add your own tests and try the program.
- To execute the project, it is necessary to have the Haskell compiler (GHC) installed on your device.
- The tests were implemented using the HUnit unit testing framework for Haskell. You can run the test file or invoke a specific function from the console.
- To run the test file:
1. If you are using an IDE, it may be necessary to open the folder with all the files (and not just the test file).
2. Open a terminal and type:
   - `ghci`
   - `:l test-catedra.hs`
   - `runCatedraTests`
- To invoke a specific function:
1. Open the terminal and navigate to the directory where the program files are located.
2. Then type:
   - `ghci`
   - `:l Solucion.hs`
   - Then, type the function name, followed by a space, input value 1, input value 2..., ensuring a space between each input value.
