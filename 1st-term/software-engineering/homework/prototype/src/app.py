from flask import Flask
from routers.main import main_bp
from routers.job_applications import job_applications_bp
from routers.feedback import feedback_bp


# Создание приложения
def create_app():
    app = Flask(__name__)

    # Регистрация роутеров
    app.register_blueprint(main_bp)
    app.register_blueprint(job_applications_bp)
    app.register_blueprint(feedback_bp)

    return app


if __name__ == "__main__":
    app = create_app()
    app.run(debug=True)
