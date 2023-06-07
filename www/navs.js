const aside = document.querySelector('.aside')
const dropdown = document.querySelectorAll('.dropdown')
const dropdownTarget = document.querySelectorAll('.dropdown-top')
const pages = document.querySelectorAll('.tab-pane')

aside.addEventListener('mouseenter', ()=>{
    aside.classList.add('active')
    links.forEach((item, i)=>{
        if(item.classList.contains('active')){
            removeActive(dropdown, 'child-active')
            openDropdown(item, 'menu-active')
            openDropdown(item, 'child-active')
        }
    })
})
aside.addEventListener('mouseleave', ()=>{
    aside.classList.remove('active')
})



dropdownTarget.forEach((item, i) => {
    item.addEventListener('click', ()=>{
        if(dropdown[i].classList.contains('menu-active')){
            dropdown[i].classList.remove('menu-active')
        } else {
            openDropdown(item, 'menu-active')
        }
    })
})

const links = document.querySelectorAll('.link')

links.forEach((item, i) => {
    item.addEventListener('click', ()=>{
        removeActive(links, 'active')
        removeActive(pages, 'active')
        item.classList.add('active')
        
        removeActive(dropdown, 'child-active')
        
        openDropdown(item, 'menu-active')
        openDropdown(item, 'child-active')

        if(links[i].getAttribute('href')){
            let att = links[i].getAttribute('href').split('#')
            document.querySelector(`#${att[1]}`).classList.add('active')
        }
    })
})



function openDropdown(el, clazze){
    if(el.tagName != 'BODY'){
        if(el.classList.contains('dropdown')){
            el.classList.add(`${clazze}`)
            openDropdown(el.parentElement, clazze)
        } else{
            openDropdown(el.parentElement, clazze)
        }
        
    } else return
}

function removeActive(items, classRemove){
    for(let j = 0; j<items.length; j++) {
        items[j].classList.remove(`${classRemove}`)
    }
}