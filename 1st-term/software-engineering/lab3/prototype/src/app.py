from flask import Flask
from flask_login import LoginManager

from injectors.services import ServicesInjector
from routers import (
    main_bp,
    customers_bp,
    users_bp,
    products_bp,
    orders_bp,
)


def create_app():
    app = Flask(__name__)
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


if __name__ == "__main__":
    app = create_app()
    app.run(host='0.0.0.0', port=80, debug=True)
