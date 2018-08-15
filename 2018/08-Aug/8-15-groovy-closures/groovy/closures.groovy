#!/usr/bin/env groovy

/**
 * Explore Closures in Groovy
 * @author Andrew Jarombek
 * @since 8/13/2018
 */

def list = ["Tod's Point", "Waveny Park", "GHS", "Mianus River Park", "Rockefeller Park"]

list.each { println it }

/**
 * Create a lottery scenario where the first item in a list has the greatest probability
 * of winning and the last item has the smallest probability.
 * @param list - a list of items, one of which will win the lottery
 * @param sortingLogic - logic to sort the list of items - remember that the first item
 * in the list has the greatest chance of winning!
 */
static def lottery(List list, Closure sortingLogic) {

    // Sort the list passed to the lottery based on the sorting logic
    def sortedList = list.sort sortingLogic

    // Determine the number of tickets in the lottery
    int tickets = 0

    (0..list.size()).each { tickets += it }

    // Pick which ticket is the winning one
    double winningTicket = Math.floor(Math.random() * tickets)

    int ticketsPicked = 0

    for (num in 0..list.size()) {
        ticketsPicked += list.size() - num
        if (ticketsPicked > winningTicket) {
            return sortedList[num]
        }
    }

    return sortedList[0]
}

/**
 * Simulate multiple lotteries (1000) to look at the trending winners of the lottery
 * @param list - a list of items, one of which will win each lottery
 * @param sortingLogic - logic to sort the list of items - the first item in the list has the
 * greatest chance of winning!
 * @return a map where the key is one of the items entered in the lottery and the key is the number
 * of times that item won the lottery
 */
static def lotterySimulator(List list, Closure sortingLogic) {

    def winners = [:]
    1000.times {
        def winner = lottery(list, sortingLogic)
        def currentWinCount = winners.getOrDefault(winner, 0)
        winners.put(winner, currentWinCount + 1)
    }

    return winners
}

// Test a lottery where the list is sorted in alphabetical order
def alphabeticalLotteryWinner = lottery(list) { a,b -> a <=> b }
println "Lottery Winner: $alphabeticalLotteryWinner"

def simulation = lotterySimulator(list, { a,b -> a <=> b })
println simulation.sort { a,b -> b.value <=> a.value }

/**
 * Class that defines methods for sorting a list entered in a lottery
 */
class SortingLogic {

    /**
     * A basic comparison function between two items
     * @param a - first item to compare
     * @param b - second item to compare
     * @return 1 if the first value is larger, 0 if they are equal, and -1 if the second value is larger
     */
    static int basic(a, b) {
        return a <=> b
    }

    /**
     * A comparison function between two items based on the number of characters in each
     * @param a - first item to compare
     * @param b - second item to compare
     * @return 1 if the first value is larger, 0 if they are equal, and -1 if the second value is larger
     */
    static int length(String a, String b) {
        return a.length() <=> b.length()
    }
}

// Test a lottery where the list is sorted in a basic order
def basicLotteryWinner = lottery(list, SortingLogic.&basic)
println "Basic Lottery Winner: $basicLotteryWinner"

def basicSimulation = lotterySimulator(list, SortingLogic.&basic)
println basicSimulation.sort { a,b -> b.value <=> a.value }

// Test a lottery where the list is sorted by the length of the string elements
def stringLengthLotteryWinner = lottery(list, SortingLogic.&length)
println "String Length Lottery Winner: $stringLengthLotteryWinner"

def stringLengthSimulation = lotterySimulator(list, SortingLogic.&length)
println stringLengthSimulation.sort { a,b -> b.value <=> a.value }