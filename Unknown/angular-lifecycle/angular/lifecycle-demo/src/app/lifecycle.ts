/**
 * Interface for a lifecycle notification of an element
 * @author Andrew Jarombek
 * @since 11/18/2018
 */

export interface Lifecycle {
    component: string;
    lifecycle: string;
    type: string;
    message?: string;
}