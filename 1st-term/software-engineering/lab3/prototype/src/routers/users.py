from injectors.services import ServicesInjector
from flask import Blueprint, request, jsonify, url_for, redirect
from flask_login import login_required

from models.user import UserLogin

users_bp = Blueprint('users', __name__, url_prefix='/users')


@users_bp.route('/', methods=['GET'])
@login_required
def get_users():
    res = [c.to_dict() for c in ServicesInjector().users().get_all()]
    return jsonify(res)


@users_bp.route('/', methods=['POST'])
def validate_user():
    login_request = UserLogin(**request.json)
    return ServicesInjector().users().login_user(login_request)
