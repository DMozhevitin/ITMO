setTimeout(run, 0)

function debounce(f, wait, immediate) {
    let timeout
    return function () {
        const ctx = this
        const args = arguments
        const later = function () {
            timeout = null
            if (!immediate) {
                f.apply(ctx, args)
            }
        }

        const callNow = immediate && !timeout
        clearTimeout(timeout)
        timeout = setTimeout(later, wait)
        if (callNow) {
            f.apply(ctx, args)
        }
    }
}

function run() {
    const ARROW_UP = 'ArrowUp'
    const ARROW_DOWN = 'ArrowDown'
    const ENTER = 'Enter'
    const URL = 'https://raw.githubusercontent.com/algolia/datasets/master/airports/airports.json'

    const search = document.getElementById('search')
    const suggested = document.getElementById('suggested-items')
    let currentSelectedItem = null

    function selectItem(selectedItem) {
        if (currentSelectedItem === selectedItem) {
            return
        }

        selectedItem.classList.add('selected')

        if (currentSelectedItem !== null) {
            currentSelectedItem.classList.remove('selected')
        }

        currentSelectedItem = selectedItem
    }

    const onItemClickHandler = (e) => {
        const selectedItem = e.target
        search.value = e.target.innerHTML
        suggested.innerHTML = ''
    }

    const onInputHandler = debounce(async (e) => {
        const query = e.target.value

        suggested.innerHTML = ''

        if (query.length === 0) {
            return
        }

        const r = await fetch(URL)
        const data = await r.json()

        const sgstd = data.filter(item =>
            query.split(' ')
                .filter(s => s.length > 0)
                .some(w => item.city.toLowerCase().startsWith(w.toLowerCase())
                    || item.iata_code.toLowerCase().startsWith(w.toLowerCase())))
            .map(item => {
                const elem = document.createElement('li')
                elem.addEventListener('click', onItemClickHandler)
                elem.addEventListener('mouseenter', onMouseEnterHandler)
                elem.innerHTML = `${item.city} ${item.iata_code}`
                return elem
            })
            .slice(0, 20)
            .forEach(elem => suggested.appendChild(elem))
    }, 300)

    const onKeydownHandler = (e) => {
        const code = e.code
        console.log('code:')
        console.log(code)
        let firstLastMapper
        let prevNextMapper

        if (code === ARROW_DOWN) {
            firstLastMapper = (s => s.firstElementChild)
            prevNextMapper = (x => x.nextElementSibling)
        } else if (code === ARROW_UP) {
            firstLastMapper = (s => s.lastElementChild)
            prevNextMapper = (x => x.previousElementSibling)
        } else if (code === ENTER) {
            if (currentSelectedItem !== null) {
                search.value = currentSelectedItem.innerHTML
                suggested.innerHTML = ''
            }

            return
        } else {
            return
        }

        let selectedItem

        if (currentSelectedItem !== null) {
            selectedItem = prevNextMapper(currentSelectedItem)
                || firstLastMapper(suggested)
            selectItem(selectedItem)
        } else {
            if (firstLastMapper(suggested) === null) {
                return
            }

            selectItem(firstLastMapper(suggested))
        }
    }

    const onMouseEnterHandler = (e) => {
        selectItem(e.target)
    }

    search.addEventListener('input', onInputHandler)
    search.addEventListener('keydown', onKeydownHandler)
}
