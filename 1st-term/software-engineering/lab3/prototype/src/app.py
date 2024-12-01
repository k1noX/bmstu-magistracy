from http.client import HTTPException

import flask
from flask import Flask, jsonify
from flask_login import LoginManager

from injectors.services import ServicesInjector
from models.exception import ModuleException
from routers import (
    main_bp,
    customers_bp,
    users_bp,
    products_bp,
    orders_bp,
)

app = Flask(__name__)


def create_app():
    app.secret_key = 'your_secret_key'

    app.register_blueprint(main_bp)
    app.register_blueprint(customers_bp)
    app.register_blueprint(users_bp)
    app.register_blueprint(products_bp)
    app.register_blueprint(orders_bp)

    login_manager = LoginManager(app)
    login_manager.login_view = '/'

    @login_manager.user_loader
    def load_user(user_id):
        return ServicesInjector().users().get_by_id(user_id)

    return app


@app.errorhandler(Exception)
def error_handler(error: Exception | ModuleException | HTTPException):
    """
    Обработчик ошибок для преобразования их в единый формат JSON.
    """
    if isinstance(error, ModuleException):
        response = flask.jsonify(error.to_dict())
        response.status_code = error.status_code
    elif isinstance(error, HTTPException):
        response = jsonify({
            "error": error.__class__.__name__,
            "data": {},
        })
        response.status_code = error.code
    else:
        response = jsonify({
            "error": "InternalServerError",
            "message": "Произошла внутренняя ошибка сервера.",
            "data": {
                'exception': error.__class__.__name__,
                'data': error.args,
            }
        })
        response.status_code = 500
    return response


if __name__ == "__main__":
    app = create_app()
    app.run(host='0.0.0.0', port=80, debug=True)
