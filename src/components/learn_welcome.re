[@mel.module "../styles/Learn.module.scss"] external css: Js.t({..}) = "default"; 

[@react.component]
let make = (~set_current_view) => {
    open Bindings;
    open Mui;

    <div className=css##welcome>
        <div style={ReactDOM.Style.make(~textAlign="center", ())}>
            <h1>
                {
                    "Welcome to the Learning page! "
                    |> React.string
                }
            </h1>
            <List>
                <ListItem 
                    secondaryAction={
                        <IconButton
                            edge=`end_
                            ariaLabel="Open Daily Vocabulary"
                            onClick={_ => set_current_view("daily_vocabulary")}
                        >
                            <TablerReact.IconArrowBigUpFilled />
                        </IconButton>
                    }
                >
                    <ListItemAvatar>
                        <Avatar>
                            <TablerReact.IconListCheck />
                        </Avatar>
                    </ListItemAvatar>
                    <ListItemText
                        primary={React.string("Build your Modern Sumerian skills with Daily Vocabulary")}
                        secondary={React.string("A 10-day challenge to learn 100 words")}
                    />
                </ListItem>
                <ListItem 
                    secondaryAction={
                        <IconButton
                            edge=`end_
                            ariaLabel="Open Lessons"
                            onClick={_ => set_current_view("lessons")}
                        >
                            <TablerReact.IconArrowBigUpFilled />
                        </IconButton>
                    }
                >
                    <ListItemAvatar>
                        <Avatar>
                            <TablerReact.IconBook2 />
                        </Avatar>
                    </ListItemAvatar>
                    <ListItemText
                        primary={React.string("Follow guided Lessons to learn the language step by step.")}
                        secondary={React.string("Lessons are structured to guide you step by step")}
                    />
                </ListItem>
                <ListItem 
                    secondaryAction={
                        <IconButton
                            edge=`end_
                            ariaLabel="Open Comics"
                            onClick={_ => set_current_view("comics")}
                        >
                            <TablerReact.IconArrowBigUpFilled />
                        </IconButton>
                    }
                >
                    <ListItemAvatar>
                        <Avatar>
                            <TablerReact.IconMickey />
                        </Avatar>
                    </ListItemAvatar>
                    <ListItemText
                        primary={React.string("Read Comics to enjoy Modern Sumerian stories.")}
                        secondary={React.string("Comics provide engaging narratives to enhance your learning")}
                    />
                </ListItem>
                <ListItem 
                    secondaryAction={
                        <IconButton
                            edge=`end_
                            ariaLabel="Open Grammar Notes"
                            onClick={_ => set_current_view("grammar_notes")}
                        >
                            <TablerReact.IconArrowBigUpFilled />
                        </IconButton>
                    }
                >
                    <ListItemAvatar>
                        <Avatar>
                            <TablerReact.IconPencil />
                        </Avatar>
                    </ListItemAvatar>
                    <ListItemText
                        primary={React.string("Read Grammar Notes to understand Sumerian grammar.")}
                        secondary={React.string("Grammar Notes provide detailed explanations and examples")}
                    />
                </ListItem>
                <ListItem 
                    secondaryAction={
                        <IconButton
                            edge=`end_
                            ariaLabel="Open Flashcards"
                            onClick={_ => set_current_view("flashcards")}
                        >
                            <TablerReact.IconArrowBigUpFilled />
                        </IconButton>
                    }
                >
                    <ListItemAvatar>
                        <Avatar>
                            <TablerReact.IconPhoto />
                        </Avatar>
                    </ListItemAvatar>
                    <ListItemText
                        primary={React.string("Use Flashcards to practise and review Sumerian vocabulary.")}
                        secondary={React.string("Flashcards help reinforce your learning")}
                    />
                </ListItem>
            </List>
        </div>
    </div>
}
