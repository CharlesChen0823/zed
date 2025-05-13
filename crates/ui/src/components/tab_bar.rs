use gpui::{AnyElement, ScrollHandle};
use smallvec::SmallVec;

use crate::Tab;
use crate::prelude::*;

#[derive(IntoElement, RegisterComponent)]
pub struct TabBar {
    id: ElementId,
    start_children: SmallVec<[AnyElement; 2]>,
    children: SmallVec<[AnyElement; 2]>,
    end_children: SmallVec<[AnyElement; 2]>,
    scroll_handle: Option<ScrollHandle>,
}

/// Frame state used by the [UniformList].
pub struct TabBarFrameState {
    start_items: SmallVec<[AnyElement; 2]>,
    items: SmallVec<[AnyElement; 2]>,
    end_items: SmallVec<[AnyElement; 2]>,
}

#[derive(Clone, Debug, Default)]
pub struct TabBarScrollHandle(pub Rc<RefCell<TabBarScrollState>>);

/// Where to place the element scrolled to.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum ScrollStrategy {
    /// Place the element at the top of the list's viewport.
    Top,
    /// Attempt to place the element in the middle of the list's viewport.
    /// May not be possible if there's not enough list items above the item scrolled to:
    /// in this case, the element will be placed at the closest possible position.
    Center,
}

#[derive(Clone, Debug, Default)]
#[allow(missing_docs)]
pub struct TabBarScrollState {
    pub base_handle: ScrollHandle,
    pub deferred_scroll_to_item: Option<(usize, ScrollStrategy)>,
    /// Size of the item, captured during last layout.
    pub last_item_size: Option<ItemSize>,
}

impl TabBarScrollHandle {
    pub fn new() -> Self {
        Self(Rc::new(RefCell::new(TabBarScrollState {
            base_handle: ScrollHandle::new(),
            deferred_scroll_to_item: None,
            last_item_size: None,
        })))
    }

    pub fn scroll_to_item(&self, ix: usize, strategy: ScrollStrategy) {
        self.0.borrow_mut().deferred_scroll_to_item = Some((ix, strategy));
    }
}

impl TabBar {
    pub fn new(id: impl Into<ElementId>) -> Self {
        Self {
            id: id.into(),
            start_children: SmallVec::new(),
            children: SmallVec::new(),
            end_children: SmallVec::new(),
            scroll_handle: None,
        }
    }

    pub fn track_scroll(mut self, scroll_handle: ScrollHandle) -> Self {
        self.scroll_handle = Some(scroll_handle);
        self
    }

    pub fn start_children_mut(&mut self) -> &mut SmallVec<[AnyElement; 2]> {
        &mut self.start_children
    }

    pub fn start_child(mut self, start_child: impl IntoElement) -> Self
    where
        Self: Sized,
    {
        self.start_children_mut()
            .push(start_child.into_element().into_any());
        self
    }

    pub fn start_children(
        mut self,
        start_children: impl IntoIterator<Item = impl IntoElement>,
    ) -> Self
    where
        Self: Sized,
    {
        self.start_children_mut().extend(
            start_children
                .into_iter()
                .map(|child| child.into_any_element()),
        );
        self
    }

    pub fn end_children_mut(&mut self) -> &mut SmallVec<[AnyElement; 2]> {
        &mut self.end_children
    }

    pub fn end_child(mut self, end_child: impl IntoElement) -> Self
    where
        Self: Sized,
    {
        self.end_children_mut()
            .push(end_child.into_element().into_any());
        self
    }

    pub fn end_children(mut self, end_children: impl IntoIterator<Item = impl IntoElement>) -> Self
    where
        Self: Sized,
    {
        self.end_children_mut().extend(
            end_children
                .into_iter()
                .map(|child| child.into_any_element()),
        );
        self
    }
}

impl ParentElement for TabBar {
    fn extend(&mut self, elements: impl IntoIterator<Item = AnyElement>) {
        self.children.extend(elements)
    }
}

impl Element for TabBar {
    type RequestLayoutState = TabBarFrameState;
    type PrepaintState = Option<Hitbox>;

    fn id(&self) -> Option<ElementId> {
        self.interactivity.element_id.clone()
    }

    fn request_layout(
        &mut self,
        global_id: Option<&GlobalElementId>,
        window: &mut Window,
        cx: &mut App,
    ) -> (LayoutId, Self::RequestLayoutState) {
        let max_items = self.item_count;
        let item_size = self.measure_item(None, window, cx);
        let layout_id = self.interactivity.request_layout(
            global_id,
            window,
            cx,
            |style, window, cx| match self.sizing_behavior {
                ListSizingBehavior::Infer => {
                    window.with_text_style(style.text_style().cloned(), |window| {
                        window.request_measured_layout(
                            style,
                            move |known_dimensions, available_space, _window, _cx| {
                                let desired_height = item_size.height * max_items;
                                let width = known_dimensions.width.unwrap_or(match available_space
                                    .width
                                {
                                    AvailableSpace::Definite(x) => x,
                                    AvailableSpace::MinContent | AvailableSpace::MaxContent => {
                                        item_size.width
                                    }
                                });
                                let height = match available_space.height {
                                    AvailableSpace::Definite(height) => desired_height.min(height),
                                    AvailableSpace::MinContent | AvailableSpace::MaxContent => {
                                        desired_height
                                    }
                                };
                                size(width, height)
                            },
                        )
                    })
                }
                ListSizingBehavior::Auto => window
                    .with_text_style(style.text_style().cloned(), |window| {
                        window.request_layout(style, None, cx)
                    }),
            },
        );

        (
            layout_id,
            UniformListFrameState {
                items: SmallVec::new(),
                decorations: SmallVec::new(),
            },
        )
    }

    fn prepaint(
        &mut self,
        global_id: Option<&GlobalElementId>,
        bounds: Bounds<Pixels>,
        frame_state: &mut Self::RequestLayoutState,
        window: &mut Window,
        cx: &mut App,
    ) -> Option<Hitbox> {
        let style = self
            .interactivity
            .compute_style(global_id, None, window, cx);
        let border = style.border_widths.to_pixels(window.rem_size());
        let padding = style
            .padding
            .to_pixels(bounds.size.into(), window.rem_size());

        let padded_bounds = Bounds::from_corners(
            bounds.origin + point(border.left + padding.left, border.top + padding.top),
            bounds.bottom_right()
                - point(border.right + padding.right, border.bottom + padding.bottom),
        );

        let can_scroll_horizontally = matches!(
            self.horizontal_sizing_behavior,
            ListHorizontalSizingBehavior::Unconstrained
        );

        let longest_item_size = self.measure_item(None, window, cx);
        let content_width = if can_scroll_horizontally {
            padded_bounds.size.width.max(longest_item_size.width)
        } else {
            padded_bounds.size.width
        };
        let content_size = Size {
            width: content_width,
            height: longest_item_size.height * self.item_count + padding.top + padding.bottom,
        };

        let shared_scroll_offset = self.interactivity.scroll_offset.clone().unwrap();
        let item_height = longest_item_size.height;
        let shared_scroll_to_item = self.scroll_handle.as_mut().and_then(|handle| {
            let mut handle = handle.0.borrow_mut();
            handle.last_item_size = Some(ItemSize {
                item: padded_bounds.size,
                contents: content_size,
            });
            handle.deferred_scroll_to_item.take()
        });

        self.interactivity.prepaint(
            global_id,
            bounds,
            content_size,
            window,
            cx,
            |style, mut scroll_offset, hitbox, window, cx| {
                let border = style.border_widths.to_pixels(window.rem_size());
                let padding = style
                    .padding
                    .to_pixels(bounds.size.into(), window.rem_size());

                let padded_bounds = Bounds::from_corners(
                    bounds.origin + point(border.left + padding.left, border.top),
                    bounds.bottom_right() - point(border.right + padding.right, border.bottom),
                );

                let y_flipped = if let Some(scroll_handle) = self.scroll_handle.as_mut() {
                    let mut scroll_state = scroll_handle.0.borrow_mut();
                    scroll_state.base_handle.set_bounds(bounds);
                    scroll_state.y_flipped
                } else {
                    false
                };

                if self.item_count > 0 {
                    let content_height =
                        item_height * self.item_count + padding.top + padding.bottom;
                    let is_scrolled_vertically = !scroll_offset.y.is_zero();
                    let min_vertical_scroll_offset = padded_bounds.size.height - content_height;
                    if is_scrolled_vertically && scroll_offset.y < min_vertical_scroll_offset {
                        shared_scroll_offset.borrow_mut().y = min_vertical_scroll_offset;
                        scroll_offset.y = min_vertical_scroll_offset;
                    }

                    let content_width = content_size.width + padding.left + padding.right;
                    let is_scrolled_horizontally =
                        can_scroll_horizontally && !scroll_offset.x.is_zero();
                    if is_scrolled_horizontally && content_width <= padded_bounds.size.width {
                        shared_scroll_offset.borrow_mut().x = Pixels::ZERO;
                        scroll_offset.x = Pixels::ZERO;
                    }

                    if let Some((mut ix, scroll_strategy)) = shared_scroll_to_item {
                        if y_flipped {
                            ix = self.item_count.saturating_sub(ix + 1);
                        }
                        let list_height = padded_bounds.size.height;
                        let mut updated_scroll_offset = shared_scroll_offset.borrow_mut();
                        let item_top = item_height * ix + padding.top;
                        let item_bottom = item_top + item_height;
                        let scroll_top = -updated_scroll_offset.y;
                        let mut scrolled_to_top = false;
                        if item_top < scroll_top + padding.top {
                            scrolled_to_top = true;
                            updated_scroll_offset.y = -(item_top) + padding.top;
                        } else if item_bottom > scroll_top + list_height - padding.bottom {
                            scrolled_to_top = true;
                            updated_scroll_offset.y = -(item_bottom - list_height) - padding.bottom;
                        }

                        match scroll_strategy {
                            ScrollStrategy::Top => {}
                            ScrollStrategy::Center => {
                                if scrolled_to_top {
                                    let item_center = item_top + item_height / 2.0;
                                    let target_scroll_top = item_center - list_height / 2.0;

                                    if item_top < scroll_top
                                        || item_bottom > scroll_top + list_height
                                    {
                                        updated_scroll_offset.y = -target_scroll_top
                                            .max(Pixels::ZERO)
                                            .min(content_height - list_height)
                                            .max(Pixels::ZERO);
                                    }
                                }
                            }
                        }
                        scroll_offset = *updated_scroll_offset
                    }

                    let first_visible_element_ix =
                        (-(scroll_offset.y + padding.top) / item_height).floor() as usize;
                    let last_visible_element_ix = ((-scroll_offset.y + padded_bounds.size.height)
                        / item_height)
                        .ceil() as usize;
                    let visible_range = first_visible_element_ix
                        ..cmp::min(last_visible_element_ix, self.item_count);

                    let items = (self.render_items)(visible_range.clone(), window, cx);

                    let content_mask = ContentMask { bounds };
                    window.with_content_mask(Some(content_mask), |window| {
                        for (mut item, ix) in items.into_iter().zip(visible_range.clone()) {
                            let item_origin = padded_bounds.origin
                                + point(
                                    if can_scroll_horizontally {
                                        scroll_offset.x + padding.left
                                    } else {
                                        scroll_offset.x
                                    },
                                    item_height * ix + scroll_offset.y + padding.top,
                                );
                            let available_width = if can_scroll_horizontally {
                                padded_bounds.size.width + scroll_offset.x.abs()
                            } else {
                                padded_bounds.size.width
                            };
                            let available_space = size(
                                AvailableSpace::Definite(available_width),
                                AvailableSpace::Definite(item_height),
                            );
                            item.layout_as_root(available_space, window, cx);
                            item.prepaint_at(item_origin, window, cx);
                            frame_state.items.push(item);
                        }

                        let bounds = Bounds::new(
                            padded_bounds.origin
                                + point(
                                    if can_scroll_horizontally {
                                        scroll_offset.x + padding.left
                                    } else {
                                        scroll_offset.x
                                    },
                                    scroll_offset.y + padding.top,
                                ),
                            padded_bounds.size,
                        );
                    });
                }

                hitbox
            },
        )
    }

    fn paint(
        &mut self,
        global_id: Option<&GlobalElementId>,
        bounds: Bounds<crate::Pixels>,
        request_layout: &mut Self::RequestLayoutState,
        hitbox: &mut Option<Hitbox>,
        window: &mut Window,
        cx: &mut App,
    ) {
        self.interactivity.paint(
            global_id,
            bounds,
            hitbox.as_ref(),
            window,
            cx,
            |_, window, cx| {
                for item in &mut request_layout.start_items {
                    item.paint(window, cx);
                }
                for item in &mut request_layout.items {
                    item.paint(window, cx);
                }
                for item in &mut request_layout.end_items {
                    item.paint(window, cx);
                }
            },
        )
    }
}

impl IntoElement for TabBar {
    type Element = Self;

    fn into_element(self) -> Self::Element {
        self
    }
}

impl InteractiveElement for TabBar {
    fn interactivity(&mut self) -> &mut crate::Interactivity {
        &mut self.interactivity
    }
}

impl RenderOnce for TabBar {
    fn render(self, _: &mut Window, cx: &mut App) -> impl IntoElement {
        div()
            .id(self.id)
            .group("tab_bar")
            .flex()
            .flex_none()
            .w_full()
            .h(Tab::container_height(cx))
            .bg(cx.theme().colors().tab_bar_background)
            .when(!self.start_children.is_empty(), |this| {
                this.child(
                    h_flex()
                        .flex_none()
                        .gap(DynamicSpacing::Base04.rems(cx))
                        .px(DynamicSpacing::Base06.rems(cx))
                        .border_b_1()
                        .border_r_1()
                        .border_color(cx.theme().colors().border)
                        .children(self.start_children),
                )
            })
            .child(
                div()
                    .relative()
                    .flex_1()
                    .h_full()
                    .overflow_x_hidden()
                    .child(
                        div()
                            .absolute()
                            .top_0()
                            .left_0()
                            .size_full()
                            .border_b_1()
                            .border_color(cx.theme().colors().border),
                    )
                    .child(
                        h_flex()
                            .id("tabs")
                            .flex_grow()
                            .overflow_x_scroll()
                            .when_some(self.scroll_handle, |cx, scroll_handle| {
                                cx.track_scroll(&scroll_handle)
                            })
                            .children(self.children),
                    ),
            )
            .when(!self.end_children.is_empty(), |this| {
                this.child(
                    h_flex()
                        .flex_none()
                        .gap(DynamicSpacing::Base04.rems(cx))
                        .px(DynamicSpacing::Base06.rems(cx))
                        .border_b_1()
                        .border_l_1()
                        .border_color(cx.theme().colors().border)
                        .children(self.end_children),
                )
            })
    }
}

impl Component for TabBar {
    fn scope() -> ComponentScope {
        ComponentScope::Navigation
    }

    fn name() -> &'static str {
        "TabBar"
    }

    fn description() -> Option<&'static str> {
        Some("A horizontal bar containing tabs for navigation between different views or sections.")
    }

    fn preview(_window: &mut Window, _cx: &mut App) -> Option<AnyElement> {
        Some(
            v_flex()
                .gap_6()
                .children(vec![
                    example_group_with_title(
                        "Basic Usage",
                        vec![
                            single_example(
                                "Empty TabBar",
                                TabBar::new("empty_tab_bar").into_any_element(),
                            ),
                            single_example(
                                "With Tabs",
                                TabBar::new("tab_bar_with_tabs")
                                    .child(Tab::new("tab1"))
                                    .child(Tab::new("tab2"))
                                    .child(Tab::new("tab3"))
                                    .into_any_element(),
                            ),
                        ],
                    ),
                    example_group_with_title(
                        "With Start and End Children",
                        vec![single_example(
                            "Full TabBar",
                            TabBar::new("full_tab_bar")
                                .start_child(Button::new("start_button", "Start"))
                                .child(Tab::new("tab1"))
                                .child(Tab::new("tab2"))
                                .child(Tab::new("tab3"))
                                .end_child(Button::new("end_button", "End"))
                                .into_any_element(),
                        )],
                    ),
                ])
                .into_any_element(),
        )
    }
}
