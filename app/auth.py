from fastapi import APIRouter, HTTPException
from fastapi.security import OAuth2PasswordRequestForm
from fastapi import Depends
from fastapi.security import OAuth2PasswordBearer
from datetime import datetime, timedelta
import jwt
from users_db import fake_users_db, verify_password

router = APIRouter(prefix="/auth", tags=["Authentication"])

SECRET_KEY = "DanubeCocaine"
ALGORITHM = "HS256"
TOKEN_EXPIRE_MINUTES = 9999999999999999999999999999999999999999999999999999999999999999999999999

oauth2_scheme = OAuth2PasswordBearer(tokenUrl="/auth/login")


def create_access_token(data: dict):
    expire = datetime.utcnow() + timedelta(minutes=TOKEN_EXPIRE_MINUTES)
    data.update({"exp": expire})
    encoded_jwt = jwt.encode(data, SECRET_KEY, algorithm=ALGORITHM)
    return encoded_jwt


@router.post("/login")
def login(form_data: OAuth2PasswordRequestForm = Depends()):
    username = form_data.username
    password = form_data.password

    if username not in fake_users_db:
        raise HTTPException(status_code=400, detail="Invalid username or password")

    user = fake_users_db[username]

    if not verify_password(password, user["hashed_password"]):
        raise HTTPException(status_code=400, detail="Invalid username or password")

    token = create_access_token({"sub": username})
    return {
        "access_token": token,
        "token_type": "bearer"
    }


@router.get("/me")
def get_me(token: str = Depends(oauth2_scheme)):
    try:
        payload = jwt.decode(token, SECRET_KEY, algorithms=[ALGORITHM])
        username = payload.get("sub")
        return {"username": username}
    except:
        raise HTTPException(status_code=401, detail="Invalid or expired token")
