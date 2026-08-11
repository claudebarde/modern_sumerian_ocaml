[@mel.module "./Footer.module.scss"] external css: Js.t({..}) = "default";
[@mel.module "./assets/bmc-logo-yellow.png"] external bmcLogoYellow: string = "default";

[@react.component]
let make = () => {
    open Bindings;
    open Mui;

    <footer>
        <div className=css##footerText>
            <p>
                {{js|© 2025 Modern Sumerian. All rights reserved.|js}|>React.string}
            </p>
        </div>
        <div>
            <Components.Meteo_widget />
        </div>
        <div>
            <a href="https://www.buymeacoffee.com/8jJNf1zyp" target="_blank">
                <img 
                    src={bmcLogoYellow} 
                    alt="Buy Me A Coffee" 
                    className={css##buyMeACoffee} 
                />
            </a>
            <Button
                variant=`contained
                className=css##donateLink
                href="https://commerce.coinbase.com/checkout/86ab3abf-c300-421f-b702-db4e58eb1bb8"
                target="_blank"
            >
                {"Donate " |> React.string}
                <TablerReact.IconCurrencyBitcoin />
            </Button>
        </div>
    </footer>
}