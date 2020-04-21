# Jammr

Make music with your pals in real time! http://alexshadley.com/jammr

## Getting started

Jammr requires a working installation of Python 3 and Elm, as well as a
js dependency bundler like Parcel.

### Backend

The backend of Jammr is a flask server using a socketio plugin. Download the
dependencies from `requirements.txt`. Run the backend with

```
python backend.py
```

### Frontend

Run your dependency bundler on `index.html` to build the frontend.
