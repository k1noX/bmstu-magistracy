from flask import Blueprint, render_template
from services.products import ProductsService

main_bp = Blueprint("main", __name__)


@main_bp.route("/")
def index():
    products = ProductsService.get_all_products()
    title = "Добро пожаловать на сайт Электроника"
    hero_title = "Лучшие решения для вашего дома и бизнеса"
    hero_description = "Обратите внимание на нашу уникальную продукцию с гарантией качества"
    hero_button_text = "Подробнее"

    return render_template(
        "index.html",
        title=title,
        hero_title=hero_title,
        hero_description=hero_description,
        hero_button_text=hero_button_text,
        products=products,
    )
