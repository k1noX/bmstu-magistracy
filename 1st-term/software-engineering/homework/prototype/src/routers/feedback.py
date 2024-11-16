from flask import Blueprint, request, redirect, url_for
from models.database import Feedback, SessionLocal

feedback_bp = Blueprint("feedback", __name__, url_prefix='/feedback')

@feedback_bp.route('/', methods=['POST'])
def submit_feedback():
    if request.method == 'POST':
        name = request.form['name']
        email = request.form['email']
        message = request.form['message']

        # Сохраняем отзыв в базе данных
        new_feedback = Feedback(name=name, email=email, message=message)

        with SessionLocal() as session:
            session.add(new_feedback)
            session.commit()

        return redirect(url_for('main.index'))