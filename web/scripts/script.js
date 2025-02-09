// =========================================================================
//  FUNCIONALIDADE: CARREGAMENTO DAS IMAGENS DINÂMICAS
// =========================================================================
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
        "./assets/images/40.webp",
        "./assets/images/41.webp",
        "./assets/images/42.webp",
        "./assets/images/43.webp",
        "./assets/images/44.webp",
        "./assets/images/45.webp",
        "./assets/images/46.webp",
        "./assets/images/47.webp",
        "./assets/images/48.webp",
        "./assets/images/49.webp",
        "./assets/images/50.webp",
        "./assets/images/51.webp",
        "./assets/images/52.webp",
        "./assets/images/53.webp",
        "./assets/images/54.webp",
        "./assets/images/55.webp",
        "./assets/images/56.webp",
        "./assets/images/57.webp",
        "./assets/images/58.webp",
        "./assets/images/59.webp",
        "./assets/images/60.webp",
        "./assets/images/61.webp",
        "./assets/images/62.webp",
        "./assets/images/63.webp",
        "./assets/images/64.webp",
        "./assets/images/65.webp",
        "./assets/images/66.webp",
        "./assets/images/67.webp",
        "./assets/images/68.webp",
        "./assets/images/69.webp",
        "./assets/images/70.webp",
        "./assets/images/71.webp",
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

// =========================================================================
//  FUNCIONALIDADE: EXIBIÇÃO/OCULTAÇÃO DO BOTÃO "IR PARA O TOPO"
// =========================================================================
window.addEventListener('scroll', function () {
    const scrollTopButton = document.getElementById('scrollTopButton');
    if (window.scrollY > 200) {
        scrollTopButton.style.display = 'block';
    } else {
        scrollTopButton.style.display = 'none';
    }
});

// =========================================================================
//  FUNCIONALIDADE: ROLAGEM SUAVE PARA O TOPO DA PÁGINA
// =========================================================================
document.getElementById('scrollTopButton').addEventListener('click', function () {
    window.scrollTo({
        top: 0,
        behavior: 'smooth'
    });
});

// =========================================================================
//  FUNCIONALIDADE: BLOQUEIO DO MENU DE CONTEXTO (CLIQUE COM O BOTÃO DIREITO)
// =========================================================================
document.addEventListener("contextmenu", function (event) {
    event.preventDefault();

    console.info(
        "Context menu (botão direito do mouse) desabilitado para proteger o conteúdo."
    );

    const mensagem = document.createElement("div");
    mensagem.textContent =
        "A funcionalidade do botão direito do mouse foi desabilitada nesta página.";

    mensagem.style.cssText = `
    position: fixed;
    top: 50%;
    left: 50%;
    transform: translate(-50%, -50%);
    background-color: rgba(0, 0, 0, 0.89);
    color: white;
    padding: 10px 20px;
    border-radius: 5px;
    z-index: 9999;
    font-size: 16px;
    `;

    document.body.appendChild(mensagem);

    setTimeout(() => {
        document.body.removeChild(mensagem);
    }, 3000);
});

// =========================================================================
//  FUNCIONALIDADE: LÓGICA PARA O TECLADO
// =========================================================================
const cards = document.querySelectorAll(".card-list");
let currentCardIndex = -1;

// =========================================================================
//  FUNÇÃO: GERENCIAMENTO DO ESTILO DE FOCO DO CARD (ADICIONA/REMOVE)
// =========================================================================
function applyFocusStyles(card) {
    card.classList.add('focus-style')
}

function removeFocusStyles(card) {
    card.classList.remove('focus-style')
}

// =========================================================================
//  FUNÇÃO: NAVEGAÇÃO ENTRE OS CARDS COM AS SETAS
// =========================================================================
function navigateCards(direction) {
    if (currentCardIndex !== -1) {
        removeFocusStyles(cards[currentCardIndex]);
    }

    if (direction === "next") {
        currentCardIndex = (currentCardIndex + 1) % cards.length;
    } else if (direction === "prev") {
        currentCardIndex = (currentCardIndex - 1 + cards.length) % cards.length;
    }
    applyFocusStyles(cards[currentCardIndex]);

    cards[currentCardIndex].focus();
}

// =========================================================================
//  FUNCIONALIDADE: EVENTO DE TECLADO PARA NAVEGAÇÃO E AÇÃO
// =========================================================================
document.addEventListener("keydown", (e) => {
    if (e.key === "ArrowRight") {
        navigateCards("next");
    } else if (e.key === "ArrowLeft") {
        navigateCards("prev");
    } else if (e.key === "Enter" && currentCardIndex !== -1) {
        const link = cards[currentCardIndex].querySelector(".card-listLink");
        if (link) {
            link.click();
        }
    }
});

// =========================================================================
//  FUNCIONALIDADE: SALVAMENTO DA POSIÇÃO DE ROLAGEM ANTES DO FECHAMENTO DA PÁGINA
// =========================================================================
window.addEventListener('beforeunload', function () {
    try {
        const scrollPosition = window.scrollY;
        if (typeof scrollPosition === 'number') {
            localStorage.setItem('scrollPosition', String(scrollPosition));
        } else {
            console.warn("Scroll position is not a number.  Not saving scroll position.");
        }
    } catch (error) {
        console.error("Error saving scroll position to localStorage:", error);
    }
});

// =========================================================================
//  FUNCIONALIDADE: RECUPERAÇÃO DA POSIÇÃO DE ROLAGEM AO CARREGAR A PÁGINA
// =========================================================================
window.addEventListener('load', function () {
    try {
        const savedScrollPosition = localStorage.getItem('scrollPosition');

        if (savedScrollPosition) {
            const parsedScrollPosition = Number(savedScrollPosition);

            if (!isNaN(parsedScrollPosition)) {
                window.scrollTo(0, parsedScrollPosition);
            } else {
                console.warn("Invalid scroll position found in localStorage:", savedScrollPosition);
                localStorage.removeItem('scrollPosition');
            }
        }
    } catch (error) {
        console.error("Error retrieving scroll position from localStorage:", error);
    }
});