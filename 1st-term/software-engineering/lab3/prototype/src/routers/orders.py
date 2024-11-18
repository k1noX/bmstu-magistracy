from flask_login import login_required, current_user

from injectors.services import ServicesInjector
from flask import Blueprint, request, jsonify, render_template

from models import OrderCreation

orders_bp = Blueprint('orders', __name__, url_prefix='/orders')


@orders_bp.route('/', methods=['GET'])
@login_required
def get_orders():
    return render_template(
        'orders.html',
        orders=ServicesInjector().orders().get_all(),
    )


@orders_bp.route('/', methods=['POST'])
@login_required
def create_order():
    creation = OrderCreation(**request.json)
    return jsonify(ServicesInjector().orders().add(creation, current_user.id))


@orders_bp.route('/', methods=['DELETE'])
@login_required
def remove_order():
    ident = int(request.args.get('id'))
    return jsonify(ServicesInjector().orders().remove(ident))


@orders_bp.route('/<int:ident>/invoice', methods=['GET'])
@login_required
def get_invoice(ident: int):
    return ServicesInjector().orders().get_invoice(ident)
