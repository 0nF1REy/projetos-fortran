"use strict";

const cardWrap = document.getElementById('card-wrap');
const cardListImages = [
    "./assets/images/01.webp",
    "./assets/images/02.webp",
    "./assets/images/03.webp"
];
let lastImageIndex = -1;

cardWrap.querySelectorAll('.card-list').forEach(card => {
    const imageElement = card.querySelector('.card-listImage img');
    let randomIndex;

    do {
        randomIndex = Math.floor(Math.random() * cardListImages.length);
    } while (randomIndex === lastImageIndex);

    imageElement.src = cardListImages[randomIndex];
    lastImageIndex = randomIndex;
});

window.addEventListener('scroll', function () {
    const scrollTopButton = document.getElementById('scrollTopButton');
    if (window.scrollY > 200) {
        scrollTopButton.style.display = 'block';
    } else {
        scrollTopButton.style.display = 'none';
    }
});

document.getElementById('scrollTopButton').addEventListener('click', function () {
    window.scrollTo({
        top: 0,
        behavior: 'smooth'
    });
});