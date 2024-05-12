# B-Tocs Python API Template for SAP ABAP 

This repository is a simple template for building Python based APIs for SAP ABAP. The python API based on FastAPI.
For the ABAP side take a look to the project B-Tocs ABAP SDK and the demo code.

## Big Picture

This project handles the following aspects:
1. Create a python based API
2. Call this API from SAP ABAP systems
3. Create a cloud native container for multi target deployment


```mermaid
flowchart LR
    subgraph cloud-native-container["Multi-Target-API"]
        pyapi4abap["Python API for ABAP Service"]

    end

    subgraph sap["SAP ABAP System"]
        sap_hello["SAP Demo for /hello - HTTP GET"]
        sap_check["SAP Demo for /check - HTTP GET with params"]
        sap_process[SAP Demo for /process - HTTP POST]
       
        subgraph sdk["B-Tocs SDK"]
            sdkcore-->pyapi4abap
        end
       
        sap_hello-->sdkcore
        sap_check-->sdkcore
        sap_process-->sdkcore
    end

 

    subgraph cloud-native-world["Deployment Target"]
        subgraph onpremise["Data Center On-Prem"]
            container_at_home
        end
        subgraph datacenter["Data Center"]
            container_at_3rd_party
        end
        subgraph hyperscaler["HyperScaler"]
            container_at_hyperscaler
        end
        subgraph sapbtp["SAP BTP"]
            container_at_sapbtp
        end
        subgraph saas["SaaS"]
            container_at_saas
        end
    end

    pyapi4abap --> container_at_home    
    pyapi4abap --> container_at_3rd_party
    pyapi4abap --> container_at_hyperscaler    
    pyapi4abap --> container_at_sapbtp
    pyapi4abap --> container_at_saas

```



## Step-By-Step Guide
1. Crete a python based API
    1. [Install requirements - python, git, vsc](doc/requirements.md)
    2. [Create a new python project](doc/new_project.md)
    3. [Check the code and API](doc/check_code_and_api.md)
    4. [Expose API to the internet](doc/expose_api.md)
2. Call this API from SAP ABAP systems 
    1. [Connect SAP to the API](doc/connect_sap.md)
    2. [Call the API endpoints from ABAP programs](doc/call_api_from_sap.md)
    3. [Enable API Debugging](doc/enable_debugging.md)
3. Create a cloud native container for multi target deployment
    1. [Build and start local docker container]()
    2. [Create a container for your api](doc/docker_container.md)
    3. [Deploy the container to a container registry]()
    4. [Use an existing container for a target platform]()

