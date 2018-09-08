#!/usr/bin/env groovy

/**
 * Dynamic method invocation in Groovy
 * @author Andrew Jarombek
 * @since 9/6/2018
 */

class RunHistory {
    List<Double> miles
    RunSettings settings

    RunHistory(miles, settings) {
        this.miles = miles
        this.settings = settings
    }

    /**
     * Add up all the miles run
     * @return total miles run in the runners history
     */
    def milesRun() {
        return miles.inject { result, i -> result + i }
    }

    /**
     * Add up all the kilometers run.  The miles list first has to be converted to kilometers
     * @return total kilometers run in the runners history
     */
    def kilometersRun() {
        return miles.collect { it * 1.60934 }
                    .inject { result, i -> result + i }
    }
}

enum Metric {
    miles, kilometers
}

class RunSettings {
    Metric metric

    RunSettings(metric) {
        this.metric = metric
    }
}

// Create a new run history object with the metric setting set to miles
def history = new RunHistory([7.73, 3.3, 2.56], [Metric.miles] as RunSettings)

// Dynamically invoke the milesRun() method based on the history settings
def milesRun = history."${history.settings.metric}Run"()

assert milesRun == 13.59

// Change the history settings to kilometers
history.settings = [Metric.kilometers] as RunSettings

// Dynamically invoke the kilometersRun() method based on the history settings
def kilometersRun = history."${history.settings.metric}Run"()

assert kilometersRun == 21.8709306