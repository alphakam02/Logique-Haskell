# Logic Reasoning for Prisoners - Haskell Project

This project implements a logical reasoning system in Haskell to help prisoners solve logical puzzles designed by a king. The prisoners are faced with a series of challenges where they must use their reasoning skills to avoid being eaten by tigers and free themselves. The program uses logical formulas to represent these challenges and checks which worlds (combinations of possible truths) satisfy them.

## Project Structure

The project consists of several Haskell files:

### 1. `CPL.hs`
This is the core module where the main functionality is implemented. It contains:

- **`Formula`**: Data type to represent logical formulas (T, F, variables, negation, conjunction, disjunction, implication, equivalence).
- **`World`**: Data type for representing possible worlds (combinations of variables).
- **Functions**:
  - `genAllWorlds`: Generates all possible worlds given a list of variables.
  - `sat`: Checks if a world satisfies a given formula.
  - `findWorlds`: Finds all worlds where a formula is satisfied.
  - Additional helper functions for formula manipulation and world generation.

### 2. `Cha1.hs` to `Cha6.hs`
These files represent the logical formulas for the six challenges that the prisoners must solve. Each of these modules exports a `challengeN` formula, which corresponds to the formula of the respective challenge (from `challenge1` to `challenge6`).

### Language
- **Haskell**: A purely functional programming language. This project utilizes its strong typing system, immutability, and recursive data structures.
- The project is designed to explore logical reasoning and formula manipulation using Haskell's powerful features.
