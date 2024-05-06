# ======================= Imports
import os
import uvicorn
from fastapi import FastAPI
from fastapi.responses import HTMLResponse
from pydantic import BaseModel

# ======================= FastAPI Configuration
app = FastAPI(
    description="B-Tocs API for ABAP Template",
    version="0.1.0"
)

# ======================= FastAPI Get Examples 
@app.get("/hello", description="Simple get call without parameters")
async def demo_simple_get():
    return {"message": "Hello SAP"}


@app.get("/check/{value}", description="Simple GET call with path and request parameters")
async def demo_get_with_params(value: int, opt_value1: str = None, opt_value2: float = None):
    try:
        return  {
                    "result": {
                        "value": value,
                        "opt_value1": opt_value1,
                        "opt_value2": opt_value2
                    } 
            }
    except Exception as exc:
        return HTMLResponse(content=f"<p>{exc}</p>", status_code=400)


# ======================= FastAPI Post Example 
class ProcessParams(BaseModel):
    value: str
    count: int = 10
    offset: int = 0


@app.post("/process", description="Simple POST request like a function module")
async def demo_post_with_model_api(data: ProcessParams):
    try:
        if data.offset <= 0 or data.count <= 0:
            return HTMLResponse(content=f"<p>wrong parameters</p>", status_code=400)
        else:
            result = []
            for index in range(data.offset, data.count):
                result.append({
                        "index": index,
                        "value": f"{data.offset}: {data.value}"
                    })
            return  result
    except Exception as exc:
        return HTMLResponse(content=f"<p>{exc}</p>", status_code=400)


# ======================= StartUp
if __name__ == "__main__":
    host = os.getenv("API_HOST", "0.0.0.0")
    port = os.getenv("API_PORT", "8000")
    uvicorn.run(app, port=int(port), host=host)

