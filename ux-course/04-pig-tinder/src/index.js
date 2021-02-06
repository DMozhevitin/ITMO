let index = 0
const count = 7

function like() {
    const badge = 'like-badge'
    document.getElementById(badge).style.display = 'block'

    swipe(badge)
}

function dislike() {
  const badge = 'dislike-badge'
  document.getElementById(badge).style.display = 'block'

  swipe(badge)
}


function superlike() {
  const badge = 'superlike-badge'
  document.getElementById(badge).style.display = 'block'

  swipe(badge)
}
function swipe(badgeToHide) {
  setTimeout(() => {
    document.getElementById('card-' + index).style.display = 'none'
    document.getElementById('card-' + (index + 1) % count).style.display = 'flex'
    index = (index + 1) % count

    document.getElementById(badgeToHide).style.display = 'none'
  }, 500)

}

document.getElementById('card-0').style.display = 'flex'


