import {EventEmitter} from "@angular/core";

/**
 * An interface for a service that emits notifications to subscribers
 * Source: https://github.com/AJarombek/mean-client-prototype/blob/master/web-app/src/app/notify.service.ts
 * @author Andrew Jarombek
 * @since 11/18/2018
 */

export interface NotifyService {

    onData: EventEmitter<any>;

    emitData(data: any);
}