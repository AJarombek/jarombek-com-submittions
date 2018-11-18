import {
    AfterContentChecked,
    AfterContentInit,
    AfterViewChecked,
    AfterViewInit,
    Component,
    DoCheck,
    OnChanges,
    OnDestroy,
    OnInit,
    ViewChild
} from '@angular/core';
import {HomeComponent} from "./home/home.component";
import {LifecycleService} from "./lifecycle.service";

/**
 * @author Andrew Jarombek
 * @since 9/18/2018
 */

@Component({
    selector: 'app-root',
    templateUrl: './app.component.html',
    styleUrls: ['./app.component.css']
})
export class AppComponent implements OnChanges, OnInit, DoCheck, AfterContentInit,
        AfterContentChecked, AfterViewInit, AfterViewChecked, OnDestroy {

    // Angular fills in viewChild with an instance of HomeComponent
    @ViewChild(HomeComponent)
    viewChild: HomeComponent;

    private LOG_TAG: string = '[App.Component]';

    /**
     * The constructor is called before all the lifecycle hook methods when the component is
     * being created.  NOTE: The components properties are not initialized in the constructor,
     * they will be initialized by the time {@code ngOnInit()} is invoked.
     */
    constructor(private lifecycleService: LifecycleService) {}

    /**
     * The first lifecycle hook method called after the constructor finishes execution.
     * It's invoked when the parent component binds a value to the components {@code @Input()}
     * properties.  If there are no input properties (like in this components case), this method
     * will never be called.  This is a change detection lifecycle hook method.
     */
    ngOnChanges(): void {
        console.info(`${this.LOG_TAG} Inside ngOnChanges`);
        const lifecycle = {
            component: this.LOG_TAG,
            lifecycle: 'ngOnChanges',
            type: 'Change'
        };

        this.lifecycleService.emitData(lifecycle);
    }

    /**
     * Called after all the components properties are initialized and {@code ngOnChanges()} has
     * finished executing.  This is a component initialization hook method.
     */
    ngOnInit(): void {
        console.info(`${this.LOG_TAG} Inside ngOnInit`);
        const lifecycle = {
            component: this.LOG_TAG,
            lifecycle: 'ngOnInit',
            type: 'Initialize'
        };

        this.lifecycleService.emitData(lifecycle);
    }

    /**
     * Custom configuration for Angular's change detection mechanism.  WARNING: This method is
     * invoked every time a change occurs, which can be costly.  This is a change detection
     * lifecycle hook method.
     */
    ngDoCheck(): void {
        console.info(`${this.LOG_TAG} Inside ngDoCheck`);
        const lifecycle = {
            component: this.LOG_TAG,
            lifecycle: 'ngDoCheck',
            type: 'Change'
        };

        this.lifecycleService.emitData(lifecycle);
    }

    /**
     * Called on a parent component after the child components state is initialized when using the
     * ng-content directive.  This is a component initialization hook method.
     */
    ngAfterContentInit(): void {
        console.info(`${this.LOG_TAG} Inside ngAfterContentInit`);
        const lifecycle = {
            component: this.LOG_TAG,
            lifecycle: 'ngAfterContentInit',
            type: 'Initialize'
        };

        this.lifecycleService.emitData(lifecycle);
    }

    /**
     * Called on a child component after it receives the content to place in the ng-content
     * directive from a parent.  This is a component initialization hook method.
     */
    ngAfterContentChecked(): void {
        console.info(`${this.LOG_TAG} Inside ngAfterContentChecked`);
        const lifecycle = {
            component: this.LOG_TAG,
            lifecycle: 'ngAfterContentChecked',
            type: 'Initialize'
        };

        this.lifecycleService.emitData(lifecycle);
    }

    /**
     * Called after all bindings on the template are complete and the components child views
     * are initialized.  This is a component initialization hook method.  This method will only
     * get called once.
     */
    ngAfterViewInit(): void {
        console.info(`${this.LOG_TAG} Inside ngAfterViewInit`);

        // Instance variables with the @ViewChild annotation first contain injected values here
        const lifecycleLength = '0' || this.viewChild.lifecycleList.length;

        const lifecycle = {
            component: this.LOG_TAG,
            lifecycle: 'ngAfterViewInit',
            type: 'Initialize',
            message: `Length of 'lifecycles' in HomeComponent: ${lifecycleLength}`
        };

        this.lifecycleService.emitData(lifecycle);
    }

    /**
     * Called when change-detection checks if there are any changes to the components template
     * bindings.  This is a change detection lifecycle hook method.  May be called multiple times
     * over the lifecycle of the component.
     */
    ngAfterViewChecked(): void {
        console.info(`${this.LOG_TAG} Inside ngAfterViewChecked`);
        const lifecycle = {
            component: this.LOG_TAG,
            lifecycle: 'ngAfterViewChecked',
            type: 'Change'
        };

        this.lifecycleService.emitData(lifecycle);
    }

    /**
     * This is a component destruction hook method.
     */
    ngOnDestroy(): void {
        console.info(`${this.LOG_TAG} Inside ngOnDestroy`);
        const lifecycle = {
            component: this.LOG_TAG,
            lifecycle: 'ngOnDestroy',
            type: 'Destroy'
        };

        this.lifecycleService.emitData(lifecycle);
    }
}
