from injectors.services import ServicesInjector
from flask import Blueprint, request, jsonify, url_for, redirect
from flask_login import login_required, login_user

users_bp = Blueprint('users', __name__, url_prefix='/users')


@users_bp.route('/', methods=['GET'])
@login_required
def get_users():
    res = [c.to_dict() for c in ServicesInjector().users().get_all()]
    return jsonify(res)


@users_bp.route('/', methods=['POST'])
def validate_user():
    username = request.form.get('username')
    password = request.form.get('password')
    user = ServicesInjector().users().validate(username, password)
    if user:
        login_user(user)
    return redirect(url_for('main.index'))
