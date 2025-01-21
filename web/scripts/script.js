document.addEventListener("DOMContentLoaded", () => {
    const imageUrls = [
        "./assets/images/01.webp",
        "./assets/images/02.webp",
        "./assets/images/03.webp",
        "./assets/images/04.webp",
        "./assets/images/05.webp",
    ];

    const images = document.querySelectorAll(".dynamic-image");

    let lastImage = null;

    images.forEach((img) => {
        let randomImage;
        do {
            randomImage = imageUrls[Math.floor(Math.random() * imageUrls.length)];
        } while (randomImage === lastImage);
        lastImage = randomImage;

        img.loading = "lazy";
        img.src = randomImage;
        img.alt = "Imagem padrão";

        img.addEventListener("error", () => {
            img.src = "./assets/images/default.webp";
            img.alt = "Imagem padrão";
        });
    });
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
