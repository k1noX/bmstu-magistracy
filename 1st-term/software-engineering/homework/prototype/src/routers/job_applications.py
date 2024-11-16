from flask import Blueprint, request, redirect, url_for, app
import os
from werkzeug.utils import secure_filename
from models.database import JobApplication, SessionLocal

job_applications_bp = Blueprint("job_applications", __name__, url_prefix='/job_applications')


ALLOWED_EXTENSIONS = {'pdf', 'doc', 'docx'}
UPLOAD_DIR = r'D:\Dev\bmstu-magistracy\1st-term\software-engineering\homework\prototype\resumes'


def allowed_file(filename):
    return '.' in filename and filename.rsplit('.', 1)[1].lower() in ALLOWED_EXTENSIONS


@job_applications_bp.route('/', methods=['POST'])
def submit_application():
    if request.method == 'POST':
        name = request.form['name']
        email = request.form['email']
        position = request.form['position']
        experience = request.form['experience']

        # Обработка загрузки резюме
        resume = request.files['resume']
        if resume and allowed_file(resume.filename):
            filename = secure_filename(resume.filename)
            resume.save(os.path.join(UPLOAD_DIR, filename))

            # Сохраняем заявку в базе данных
            new_application = JobApplication(
                name=name,
                email=email,
                position=position,
                experience=experience,
                resume_filename=filename
            )

            with SessionLocal() as session:
                session.add(new_application)
                session.commit()

            return redirect(url_for('main.index'))
