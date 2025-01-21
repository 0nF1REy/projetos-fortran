"use strict";

const CARD_WRAP_ID = 'card-wrap';
const ERROR_IMAGE_PATH = './assets/images/erro_imagem.png';
const CARD_LIST_IMAGES = [
    "./assets/images/01.webp",
    "./assets/images/02.webp",
    "./assets/images/03.webp",
    "./assets/images/04.webp",
    "./assets/images/05.webp"
];

let shuffledImages = [];
let currentImageIndex = 0;

function shuffleArray(array) {
    const shuffled = [...array];
    for (let i = shuffled.length - 1; i > 0; i--) {
        const j = Math.floor(Math.random() * (i + 1));
        [shuffled[i], shuffled[j]] = [shuffled[j], shuffled[i]];
    }
    return shuffled;
}

function getNextImage() {
    if (currentImageIndex >= shuffledImages.length) {
        shuffledImages = shuffleArray(CARD_LIST_IMAGES);
        currentImageIndex = 0;
    }
    return shuffledImages[currentImageIndex++];
}

function loadImage(imageElement, imageUrl, cardTitle) {
    imageElement.onload = () => {
    };

    imageElement.onerror = () => {
        console.error(`Erro ao carregar imagem: ${imageUrl} do card ${cardTitle}`);
        imageElement.src = ERROR_IMAGE_PATH;
    };

    imageElement.src = imageUrl;
}

function initImageLoading() {
    const cardWrap = document.getElementById(CARD_WRAP_ID);
    if (!cardWrap) {
        console.error(`Elemento com ID '${CARD_WRAP_ID}' não encontrado no DOM.`);
        return;
    }

    shuffledImages = shuffleArray(CARD_LIST_IMAGES);

    cardWrap.querySelectorAll('.card-list').forEach(card => {
        const imageElement = card.querySelector('.card-listImage img');
        const cardTitle = card.querySelector(".card-listTitle")?.textContent || 'Card sem título';
        const nextImageUrl = getNextImage();
        loadImage(imageElement, nextImageUrl, cardTitle);
    });
}

initImageLoading();

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