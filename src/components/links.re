[@mel.module "../styles/Links.module.scss"] external css: Js.t({..}) = "default"; 

[@react.component]
let make = () => {
    open Bindings;
    open Mui;

    let iconSize = 24;

    <div className=css##links>
        <Typography variant=Typography.Variant.body1>
            {"This is a list of links to other resources that may be useful for learning more about Sumerian:" |> React.string}
        </Typography>
        <List component=RootComponent.htmlElement("nav")>
            <ListItem 
                component=RootComponent.htmlElement("a") 
                href="https://oracc.museum.upenn.edu/epsd2/sux" 
                target="_blank"
                disablePadding=true
            >
                <ListItemButton>
                    <ListItemAvatar>
                        <TablerReact.IconBook2 size=iconSize />
                    </ListItemAvatar>
                    <ListItemText 
                        primary={"EPSD2 Dictionary" |> React.string} 
                        secondary={"Electronic Pennsylvania Sumerian Dictionary 2" |> React.string} 
                    />
                </ListItemButton>
            </ListItem>
            <ListItem 
                component=RootComponent.htmlElement("a") 
                href="https://www.facebook.com/ModernSumerian" 
                target="_blank"
                disablePadding=true
            >
                <ListItemButton>
                    <ListItemAvatar>
                        <TablerReact.IconBrandFacebook size=iconSize />
                    </ListItemAvatar>
                    <ListItemText 
                        primary={"Modern Sumerian Facebook" |> React.string} 
                        secondary={"The official Facebook page of the Modern Sumerian project" |> React.string} 
                    />
                </ListItemButton>
            </ListItem>
            <ListItem 
                component=RootComponent.htmlElement("a") 
                href="https://x.com/EmegirUmee" 
                target="_blank"
                disablePadding=true
            >
                <ListItemButton>
                    <ListItemAvatar>
                        <TablerReact.IconBrandX size=iconSize />
                    </ListItemAvatar>
                    <ListItemText 
                        primary={"Modern Sumerian X" |> React.string} 
                        secondary={"The official X page of the Modern Sumerian project" |> React.string} 
                    />
                </ListItemButton>
            </ListItem>
            <ListItem 
                component=RootComponent.htmlElement("a") 
                href="https://home.zcu.cz/~ksaskova/Sign_List.html" 
                target="_blank"
                disablePadding=true
            >
                <ListItemButton>
                    <ListItemAvatar>
                        <TablerReact.IconList size=iconSize />
                    </ListItemAvatar>
                    <ListItemText 
                        primary={"Cuneiform Sign List" |> React.string} 
                        secondary={"A list of cuneiform signs" |> React.string} 
                    />
                </ListItemButton>
            </ListItem>
            <ListItem 
                component=RootComponent.htmlElement("a") 
                href="http://psd.museum.upenn.edu/nepsd-frame.html" 
                target="_blank"
                disablePadding=true
            >
                <ListItemButton>
                    <ListItemAvatar>
                        <TablerReact.IconBook2 size=iconSize />
                    </ListItemAvatar>
                    <ListItemText 
                        primary={"EPSD1 Dictionary" |> React.string} 
                        secondary={"Electronic Pennsylvania Sumerian Dictionary, version 1" |> React.string} 
                    />
                </ListItemButton>
            </ListItem>
            <ListItem 
                component=RootComponent.htmlElement("a") 
                href="https://etcsl.orinst.ox.ac.uk/#" 
                target="_blank"
                disablePadding=true
            >
                <ListItemButton>
                    <ListItemAvatar>
                        <TablerReact.IconFileText size=iconSize />
                    </ListItemAvatar>
                    <ListItemText 
                        primary={"ETCSL" |> React.string} 
                        secondary={"The Electronic Text Corpus of Sumerian Literature" |> React.string} 
                    />
                </ListItemButton>
            </ListItem>
            <ListItem 
                component=RootComponent.htmlElement("a") 
                href="https://en.wikipedia.org/wiki/Cuneiform_(Unicode_block)" 
                target="_blank"
                disablePadding=true
            >
                <ListItemButton>
                    <ListItemAvatar>
                        <TablerReact.IconKeyboard size=iconSize />
                    </ListItemAvatar>
                    <ListItemText 
                        primary={"Cuneiform Unicode Blocks" |> React.string} 
                        secondary={"The Unicode blocks for Sumerian cuneiforms" |> React.string} 
                    />
                </ListItemButton>
            </ListItem>
        </List>
    </div>
}