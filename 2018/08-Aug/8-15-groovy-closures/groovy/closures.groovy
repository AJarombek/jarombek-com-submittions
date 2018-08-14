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
 * @param list
 * @param sortingLogic
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

def alphabeticalLottery = lottery(list) { a,b -> a <=> b }
println "Lottery Winner: $alphabeticalLottery"