# Debugging

In this step the current api service will be debugged. This is very helpful to find errors between the calling process - e.g. SAP - and the service.

## Set a breakpoint

To stop a program at a specific code line a breakpint is required. In VSC a click at the left border is used to set a breakpoint. See the screenshot for a breakpoint in API method "/check".

![VSC set breakpoint](res/vsc_set_breakpoint.jpg).

## Start the service in debugging mode

Stop the current service process via CTRL-C if running. Then start the service in debugging mpde via menu "Run - Start Debugging".

![VSC start debugging](res/vsc_start_debugging.jpg)

Select the python debugger.

![VSC select debugger](res/vsc_select_debugger.jpg)

Select the debug configuration "Python file".

![VSC select confuguration](res/vsc_debug_configurations.jpg)

Then the service will bet started in debug mode.

![VSC debugger started](res/vsc_debugger_started.jpg)

## Call the API and debug

Now the API method can be called to activate the debugger. If no remote system is involved the Swagger UI can be used.

![Swagger UI - trigger debugger](res/swagger_ui_trigger_breakpoint.jpg)

The debugger is triggered and can be used to check the code and find bugs.

![VSC Debugger](res/vsc_debugger_active.jpg)

## Summary 

You know how the VSC python debugger is used.