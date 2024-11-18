from flask_login import login_required

from injectors.services import ServicesInjector
from flask import Blueprint, request, jsonify, render_template

from models import CustomerCreation

customers_bp = Blueprint('customers', __name__, url_prefix='/customers')


@customers_bp.route('/', methods=['GET'])
@login_required
def get_customers():
    return render_template(
        'customers.html',
        customers=ServicesInjector().customers().get_all(),
    )


@customers_bp.route('/', methods=['POST'])
@login_required
def create_customer():
    creation = CustomerCreation(**request.json)
    return jsonify(ServicesInjector().customers().add(creation))


@customers_bp.route('/', methods=['DELETE'])
@login_required
def remove_customer():
    ident = int(request.args.get('id'))
    return jsonify(ServicesInjector().customers().remove(ident))


@customers_bp.route('/<int:ident>/', methods=['GET'])
@login_required
def get_customer(ident: int):
    return jsonify(ServicesInjector().customers().get_by_id(ident))
