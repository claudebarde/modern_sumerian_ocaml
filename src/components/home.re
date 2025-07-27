[@mel.module "../styles/Home.module.scss"] external css: Js.t({..}) = "default"; 

[@react.component]
let make = () => {
    <div className={css##home}>
        <div className={css##intro}>
            <p>{"Welcome to Modern Sumerian!"|> React.string}</p>
            <p>{"Modern Sumerian is your gateway to exploring the ancient and fascinating Sumerian language!"|> React.string}</p>
            <p>{"Dive into our interactive verb conjugator to master Sumerian grammar"|> React.string} </p>
            <p>{"Stay tuned for our upcoming dictionary, lessons, and more tools designed to bring this ancient language to life."|> React.string}</p>
            <p>{"Whether you're a scholar, enthusiast, or curious learner, Modern Sumerian is here to help you unlock the secrets of humanity's oldest written language."|> React.string}</p>
        </div>
    </div>
}