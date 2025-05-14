[@mel.module "../styles/Links.module.scss"] external css: Js.t({..}) = "default"; 

[@react.component]
let make = () => {
    <div className=css##links>
        <div>
            <p>
                {"This is a list of links to other resources that may be useful for learning more about Sumerian:" |> React.string}
            </p>
            <ul>
                <li>
                    <a href="https://oracc.museum.upenn.edu/epsd2/sux" target="_blank">
                        {"Electronic Pennsylvania Sumerian Dictionary 2" |> React.string}
                    </a>
                </li>
                <li>
                    <a href="https://www.facebook.com/ModernSumerian" target="_blank">
                        {"The official Facebook page of the Modern Sumerian project" |> React.string}
                    </a>
                </li>
                <li>
                    <a href="https://x.com/EmegirUmee" target="_blank">
                        {"The official X page of the Modern Sumerian project" |> React.string}
                    </a>
                </li>
                <li>
                    <a href="https://home.zcu.cz/~ksaskova/Sign_List.html" target="_blank">
                        {"A list of cuneiform signs" |> React.string}
                    </a>
                </li>
                <li>
                    <a href="https://qantuppi.web.app/" target="_blank">
                        {"The Qantuppi app (to write cuneiforms)" |> React.string}
                    </a>
                </li>
                <li>
                    <a href="http://psd.museum.upenn.edu/nepsd-frame.html" target="_blank">
                        {"Electronic Pennsylvania Sumerian Dictionary, version 1" |> React.string}
                    </a>
                </li>
                <li>
                    <a href="https://etcsl.orinst.ox.ac.uk/#" target="_blank">
                        {"The Electronic Text Corpus of Sumerian Literature" |> React.string}
                    </a>
                </li>
                <li>
                    <a href="https://en.wikipedia.org/wiki/Cuneiform_(Unicode_block)" target="_blank">
                        {"The Unicode blocks for Sumerian cuneiforms" |> React.string}
                    </a>
                </li>
            </ul>
        </div>
    </div>
}