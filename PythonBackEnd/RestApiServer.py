import flask
from flask import request, jsonify
import subprocess
from flask_cors import CORS, cross_origin
import json
import os

app = flask.Flask(__name__)
app.config["DEBUG"] = True
cors = CORS(app)
app.config['CORS_HEADERS'] = 'Content-Type'

chapel_frontend_executable = "/home/ubuntu/chapel_frontend/ChapelParseJson"
chapel_frontend_param = "/tmp/program_to_be_parsed.txt"

@app.route('/', methods=['GET'])
def home():
    return "<h1>Home</h1><p>Here you cant see nothing..</p>"

@app.route('/api/v1/resources/haskell/chapel_parser/', methods=['POST'])
@cross_origin()
def api_all():
    post_content = request.get_json()
    program = post_content["program"]

    if os.path.exists(chapel_frontend_param):
        os.remove(chapel_frontend_param)


    file = open(chapel_frontend_param,"w+")

    file.write(program)

    file.close()

    temp = subprocess.Popen([chapel_frontend_executable, chapel_frontend_param], stdout = subprocess.PIPE, universal_newlines=True) 
    # get the output as a string
    output = str(temp.communicate()[0])

    print (output)
    
    # response = "asdad"
    # response.headers.add("Access-Control-Allow-Origin", "*")

    # response.headers.add('Access-Control-Allow-Origin', '*')
    # print (output)
    return output

app.run(host='0.0.0.0', port=8080)