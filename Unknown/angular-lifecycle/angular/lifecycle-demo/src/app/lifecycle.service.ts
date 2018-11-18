import {NotifyService} from "./notify.service";
import {EventEmitter, Injectable} from "@angular/core";
import {Lifecycle} from "./lifecycle";

/**
 * A service that emits notifications when a lifecycle occurs
 * @author Andrew Jarombek
 * @since 11/18/2018
 */

@Injectable()
export class LifecycleService implements NotifyService {
    public onData: EventEmitter<any> = new EventEmitter<Lifecycle>();

    public emitData(data: Lifecycle) {
        this.onData.emit(data);
    }
}