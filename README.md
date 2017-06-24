# Train Reservation Kata (Haskell)

## Description of the Kata

An implementation of a problem close to the challenged proposed in:
https://github.com/emilybache/KataTrainReservation

The goal is to implement the domain of train reservation:
* We should offer an API to reserve a given number of seats at a given date
* The domain relies on Service Provider Interfaces (SPI)
* The implementation should make sure to be as decoupled as possible to the SPI

The rules of the reservation of a train are the following:
* We cannot reserve seats in a train if it bumps up the occupancy over 70%
* All the reserved seats should be in the same coach (we cannot separate families)
* Preferably, we should avoid bumping the occupancy of a coach over 70%

## Design and Implementation

This part describes the design choices in Haskell to implement the Train Reservation Kata, following a Domain Driven Design approach.

### Motivation

The motivation is to demonstrate how good Functional Programming languages are at modeling a domain problem. It follows from some critics I heard, saying FP is not appropriate for Domain Driven Design, based on the following arguments:
* The applicability of the Hexagonal Architecture in FP
* The fact that FP encourages anemic objects (feature envy)
* The inability of FP to represent entity objects (identities)

The motivation is to show that FP offers its own way to deal with problem decomposition.

### Chosen Design

The chosen design is based on a small Domain Specific Language (DSL), which as the name says, is pretty indicated to offer an evaluation context for a given domain (problem space).

The DSL implements the high level rules of the reservation of a train:
* The types mapping the bounded context language (mainly data)
* The abstract specification of the interact with the other domains (such as the ability to retrieve an topology of a train)

The implementation of the reservation service is then done inside the DSL, ensuring that:
* The developer cannot cheat: only the interactions allowed by the DSL are available
* The rules of the domain are enforced: the code cannot compile if it violates some invariants
* Independence with SPI (high decoupling) and easy testability (see interpreter below)

The Interpreter is able to transform an expression of the DSL into real world code:
* It handles the implementation details of the communication with the SPIs
* It can handle the exceptions of this communication (exceptions do not traverse the domain code!)
* It can be replaced by fake interpreters to: test, mock, simulate, etc.

### Advantages of the Hexagonal Architecture

The decoupling is greater:
* The rules of the domain are separated from the details of the implementation
* Error handling does not have to traverse the domain code (who does not even see them)
* The DSL makes sure we can interpret the code in completely other ways

The type safety is stronger:
* We can enforce a workflow in the DSL by using the type system (making sure to validate a reservation for instance)
* The code running in the DSL just cannot break the rules and connect to random services (stronger dependencies insurances)

## Conclusion

As much as some of the tactical patterns of the Blue Book are not directly applicable to pure Functional Programming languages, other patterns are available in Haskell that offer different trade-offs (and potentially stronger domain invariants).
