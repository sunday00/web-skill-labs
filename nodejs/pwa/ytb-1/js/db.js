import {
    addDoc,
    collection,
    deleteDoc,
    doc,
    onSnapshot
} from "https://www.gstatic.com/firebasejs/12.18.0/firebase-firestore.js"

const db = window.db


onSnapshot(collection(db, 'contacts'), (s) => {
    s.docChanges().forEach((chg) => {
        if (chg.type === 'added') {
            window.renderContact(chg.doc.data(), chg.doc.id)
        }

        if (chg.type === 'removed') {
            removeContact(chg.doc.id)
        }
    })
})


const form = document.querySelector('form');

form.addEventListener('submit', (e) => {
    e.preventDefault()

    const contact = {
        name: form.name.value,
        number: form.numbers.value
    }

    addDoc(collection(db, 'contacts'), contact).catch(console.error)

    form.name.value = ''
    form.numbers.value = ''
})

const contactContainer = document.querySelector('.contacts')
contactContainer.addEventListener('click', (e) => {
    if (e.target.tagName === 'I') {
        const id = e.target.getAttribute('data-id')
        deleteDoc(doc(db, 'contacts', id))
    }
})