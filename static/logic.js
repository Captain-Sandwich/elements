function cardNode(x) {
    var div = document.createElement("div");
    switch(x) {
    case 1:
        div.className = "card open one";
        div.textContent = "1";
        break;
    case 2:
        div.className = "card open two";
        div.textContent = "2";
        break;
    case 3:
        div.className = "card open three";
        div.textContent = "3";
        break;
    case 4:
        div.className = "card open four";
        div.textContent = "4";
        break;
    case 5:
        div.className = "card open five";
        div.textContent = "5";
        break;
    case 6:
        div.className = "card open six";
        div.textContent = "6";
        break;
    case 0:
        div.className = "card hidden";
        div.textContent = "ELEMENTS";
    };
    return div
};

function popCard(x) {
    var hand = document.getElementById("hand_player");
    children = hand.children;
    if (x >= 0 && x < children.length) {
        card = children.item(x);
        hand.removeChild(card);
        return true;
    }
    else {return false;};
};

function getElementIndex(node) {
    var index = 0;
    while ( (node = node.previousElementSibling) ) {
        index++;
    }
    return index;
}

function popThis(obj) {
    index = getElementIndex(obj);
    parent = obj.parentNode;
    parent.removeChild(obj);
    return index;
}

function clickCard() {
    index = popThis(this); // then use index to check logic with server
    state.playerCards.splice(index, 1)
    console.log(index);
    console.log(state.playerCards)
    updateAll();
}

function addHidden(x) {
    newCard = cardNode(0);
    var element = document.getElementById("hand_hidden");
    element.appendChild(newCard);
    return true;
};

function addMiddle(x) {
    state.middleCards.push(x)
    newCard = cardNode(x);
    newCard.addEventListener("click", clickCard);
    var middle = document.getElementById("middle");
    middle.appendChild(newCard);
    updateAll();
    return true;
}

function addCard(x) {
    if (x >= 1 && x <=6){
    state.playerCards.push(x)
    newCard = cardNode(x);
    newCard.addEventListener("click", clickCard);
    var element = document.getElementById("hand_player");
    element.appendChild(newCard);
    updateAll();
    return true;
    }
    else {
        return false;
    }
};

function sum(array){
    return array.reduce( (prev, curr) => prev + curr , 0);
}

function scorePlayer(x) {
    box = document.getElementById("score player");
    box.textContent = x.toString();
}

function scoreMiddle(x) {
    box = document.getElementById("score middle");
    box.textContent = x.toString();
}

function randomCard() {
    x = Math.floor(Math.random() * 6);
    addCard(x);
};

function updateAll(){
    scorePlayer(sum(state.playerCards));
    scoreMiddle(sum(state.middleCards));
}

function post(obj) {
    var xmlhttp = new XMLHttpRequest();
    //xmlhttp.open("GET", "/json-handler");
    xmlhttp.open("GET", "/logic.js");
//    xmlhttp.setRequestHeader("Content-Type", "application/json;charset=UTF-8");
    //    xmlhttp.send(JSON.stringify(obj))
}

var state = {};
state.playerCards = [];
state.middleCards = [];
state.playerScore = 0;
state.middleScore = 0;

window.onload = function(){
    addCard(1);
    addCard(2);
    addCard(3);
    addCard(4);
    addCard(5);
    addCard(6);
    addCard(1);
    addHidden();
    addHidden();
    addHidden();
    addHidden();
    addMiddle(3);
    addMiddle(5);
    btn = document.getElementById('add');
    btn.addEventListener("click", randomCard);
}
