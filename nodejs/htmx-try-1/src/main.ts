import htmx from 'htmx.org'
import Mustache from 'mustache'
import 'htmx-ext-client-side-templates'
import '@/styles/index.css'

htmx.config.selfRequestsOnly = false

// @ts-ignore
window.htmx = htmx
window.Mustache = Mustache

console.log(import.meta.env.VITE_API_HOST)

document.querySelector('#app')!.innerHTML = `
<div hx-ext="client-side-templates">
  <div class="sample-transition">
    <button hx-get="${import.meta.env.VITE_API_HOST}/user?hi=1"
     hx-trigger="click"
     hx-swap="innerHTML transition:true"
     hx-target="#content"
     mustache-template="foo"
    >
     Click Me!
    </button>
    
    <p id="content">Start</p>
    
    <template id="foo">
      <div> {{#items}}
        <p>{{id}}</p>
       {{/items}}</div>
    </template>
  </div>
</div>
`
