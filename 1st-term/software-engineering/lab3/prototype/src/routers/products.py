from flask_login import login_required

from injectors.services import ServicesInjector
from flask import Blueprint, request, jsonify, render_template

from models import ProductCreation

products_bp = Blueprint('products', __name__, url_prefix='/products')


@products_bp.route('/', methods=['GET'])
@login_required
def get_products():
    return render_template(
        'products.html',
        products=ServicesInjector().products().get_all(),
    )


@products_bp.route('/', methods=['POST'])
@login_required
def create_product():
    creation = ProductCreation(**request.json)
    return jsonify(ServicesInjector().products().add(creation))


@products_bp.route('/', methods=['DELETE'])
@login_required
def remove_product():
    ident = int(request.args.get('id'))
    return jsonify(ServicesInjector().products().remove(ident))


@products_bp.route('/<int:ident>/', methods=['GET'])
@login_required
def get_product(ident: int):
    return jsonify(ServicesInjector().products().get_by_id(ident))
