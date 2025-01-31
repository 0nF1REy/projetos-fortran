document.addEventListener("DOMContentLoaded", () => {
    const imageUrls = [
        "./assets/images/01.webp",
        "./assets/images/02.webp",
        "./assets/images/03.webp",
        "./assets/images/04.webp",
        "./assets/images/05.webp",
        "./assets/images/06.webp",
        "./assets/images/07.webp",
        "./assets/images/08.webp",
        "./assets/images/09.webp",
        "./assets/images/10.webp",
        "./assets/images/11.webp",
        "./assets/images/12.webp",
        "./assets/images/13.webp",
        "./assets/images/14.webp",
        "./assets/images/15.webp",
        "./assets/images/16.webp",
        "./assets/images/17.webp",
        "./assets/images/18.webp",
        "./assets/images/19.webp",
        "./assets/images/20.webp",
        "./assets/images/21.webp",
        "./assets/images/22.webp",
        "./assets/images/23.webp",
        "./assets/images/24.webp",
        "./assets/images/25.webp",
        "./assets/images/26.webp",
        "./assets/images/27.webp",
        "./assets/images/28.webp",
        "./assets/images/29.webp",
        "./assets/images/30.webp",
        "./assets/images/31.webp",
        "./assets/images/32.webp",
        "./assets/images/33.webp",
        "./assets/images/34.webp",
        "./assets/images/35.webp",
        "./assets/images/36.webp",
        "./assets/images/37.webp",
        "./assets/images/38.webp",
        "./assets/images/39.webp",
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
            img.loading = "lazy";
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

document.addEventListener("contextmenu", function (e) {
    e.preventDefault();
});

