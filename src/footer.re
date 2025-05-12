[@mel.module "./Footer.module.scss"] external css: Js.t({..}) = "default"; 

[@react.component]
let make = () => {
    <footer>
        <div>
            <p>
                {{js|© 2025 Modern Sumerian. All rights reserved.|js}|>React.string}
            </p>
        </div>
        <div>
            <a href="https://www.buymeacoffee.com/8jJNf1zyp" target="_blank">
                <img 
                    src="https://cdn.buymeacoffee.com/buttons/v2/default-yellow.png" 
                    alt="Buy Me A Coffee" 
                    className={css##buyMeACoffee} 
                />
            </a>
            <a href="https://commerce.coinbase.com/checkout/86ab3abf-c300-421f-b702-db4e58eb1bb8" target="_blank">
                {"Crypto donations"|>React.string}
            </a>
        </div>
    </footer>
}